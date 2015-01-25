#!/usr/bin/env perl6
use v6;

# This script isn't in bin/ because it's not meant to be installed.

BEGIN say 'Initializing ...';

use Pod::To::HTML;
use URI::Escape;
use lib 'lib';
use Perl6::TypeGraph;
use Perl6::TypeGraph::Viz;
use Perl6::Documentable::Registry;
use Pod::Convenience;

my $*DEBUG = False;

my $tg;
my %methods-by-type;
my Bool %written;

my $DR = Perl6::Documentable::Registry.new;

# Pod::To::HTML can only handle a single thread right now
my $writer = Supply.new;
$writer.act: {
    spurt .[0], p2h |.[1];
    say "Wrote ", .[0], " 'cause of line ", .[2]
}
sub write-pod ($filename, $pod, $section?) {
    $writer.emit([$filename, [$pod, $section//Nil], Backtrace.new[3].line])
}

sub url-munge($_) {
    return $_ if m{^ <[a..z]>+ '://'};
    return "/type/{uri_escape $_}" if m/^<[A..Z]>/;
    return "/routine/{uri_escape $_}" if m/^<[a..z]>|^<-alpha>*$/;
    # poor man's <identifier>
    if m/ ^ '&'( \w <[[\w'-]>* ) $/ {
        return "/routine/{uri_escape $0}";
    }
    return $_;
}

# TODO: Generate menulist automatically
my @menu =
    ('language',''         ) => (),
    ('type', 'Types'       ) => <basic composite domain-specific exceptions>,
    ('routine', 'Routines' ) => <sub method term operator>,
#    ('module', 'Modules'   ) => (),
#    ('formalities',''      ) => ();
;
        
my $head   = slurp 'template/head.html';
my $footer = footer-html;
sub header-html ($current-selection = 'nothing selected') is cached {
    state $header = slurp 'template/header.html';

    my $menu-items = [~]
        q[<div class="menu-items dark-green">],
        @menu>>.key.map({qq[
            <a class="menu-item {.[0] eq $current-selection ?? "selected darker-green" !! ""}"
                href="/{.[0]}.html">
                { .[1] || .[0].wordcase }
            </a>
        ]}), #"
        q[</div>];

    my $sub-menu-items = '';
    state %sub-menus = @menu>>.key>>[0] Z=> @menu>>.value;
    if %sub-menus{$current-selection} -> $_ {
        $sub-menu-items = [~] 
            q[<div class="menu-items darker-green">],
            qq[<a class="menu-item" href="/$current-selection.html">All</a>],
            .map({qq[
                <a class="menu-item" href="/$current-selection\-$_.html">
                    {.wordcase}
                </a>
            ]}),
            q[</div>]
    }

    $header.subst('MENU', qq[
        <div class="menu">
        $menu-items
        $sub-menu-items
        </div>
    ])

}

sub p2h($pod, $selection = 'nothing selected') {
    pod2html $pod,
        :url(&url-munge),
        :$head,
        :header(header-html $selection),
        :$footer,
        :default-title("Perl 6 Documentation"),
}

sub recursive-dir($dir) {
    my @todo = $dir;
    gather while @todo {
        my $d = @todo.shift;
        for dir($d) -> $f {
            if $f.f {
                take $f;
            }
            else {
                @todo.push($f.path);
            }
        }
    }
}

sub svg-for-file($file) {
    my $handle = open $file;
    my $str = join "\n", grep { /^'<svg'/ ff False }, $handle.lines;
    $handle.close;
    $str;
}

# --sparse=5: only process 1/5th of the files
# mostly useful for performance optimizations, profiling etc.
sub MAIN(
    Bool :$debug,
    Bool :$typegraph = False,
    Int  :$sparse,
    Bool :$disambiguation = True,
    Bool :$search-file = True,
) {
    $*DEBUG = $debug;

    say 'Creating html/ subdirectories ...';
    for '', <type language routine images syntax> {
        mkdir "html/$_" unless "html/$_".IO ~~ :e;
    }

    say 'Reading type graph ...';
    $tg = Perl6::TypeGraph.new-from-file('type-graph.txt');
    my %h = $tg.sorted.kv.flat.reverse;
    write-type-graph-images(:force($typegraph));

    $DR.lookup('type', :by<kind>).tap: {
        .tap({
            %written{$_.name} = False;
            write-type-source $_;
            %written{$_.name} = True;
        })
    }

    tap-disambiguation-files if $disambiguation;
    tap-search-file          if $search-file;
    tap-index-files;

    for <routine syntax> -> $kind {
        tap-kind $kind;
    }

    process-pod-dir 'Type', :sorted-by{ %h{.key} // -1 }, :$sparse;
    #process-pod-dir 'Language', :$sparse;

    say 'Composing doc registry ...';
    $DR.compose;

    say 'Processing complete.';
    if $sparse || !$search-file || !$disambiguation {
        say "This is a sparse or incomplete run. DO NOT SYNC WITH doc.perl6.org!";
    }
}

sub process-pod-dir($dir, :&sorted-by = &[cmp], :$sparse) {
    say "Reading lib/$dir ...";
    my @pod-sources =
        recursive-dir("lib/$dir/")\
        .grep({.path ~~ / '.pod' $/})\
        .map({;
            .path.subst("lib/$dir/", '')\
                 .subst(rx{\.pod$},  '')\
                 .subst(:g,    '/',  '::')
            => $_
        }).sort(&sorted-by);
    if $sparse {
        @pod-sources = @pod-sources[{^($_ / $sparse).ceiling}];
    }

    say "Processing $dir Pod files ...";
    my $total = +@pod-sources;
    my $what  = $dir.lc;
    my &processor = $what eq "language"
        ?? &process-language-source
        !! &process-type-source;
    for @pod-sources.kv -> $num, (:key($podname), :value($file)) {
        printf "% 4d/%d: % -40s => %s\n", $num+1, $total, $file.path, "$what/$podname";
        my $pod  = EVAL(slurp($file.path) ~ "\n\$=pod")[0];
        processor :$pod, :$podname, :pod-is-complete;
    }
}

sub process-language-source(:$pod, :$podname, :$pod-is-complete) {
    my $name = $podname;
    my $summary = '';
    if $pod.contents[0] ~~ {$_ ~~ Pod::Block::Named and .name eq "TITLE"} {
        $name = $pod.contents[0].contents[0].contents[0]
    } else {
        note "$podname does not have an =TITLE";
    }
    if $pod.contents[1] ~~ {$_ ~~ Pod::Block::Named and .name eq "SUBTITLE"} {
        $summary = $pod.contents[1].contents[0].contents[0];
    } else {
        note "$podname does not have an =SUBTITLE";
    }
    my $origin = $DR.add-new(
        :kind<language>,
        :name($name),
        :url("/language/$podname"),
        :$summary,
        :$pod,
        :pod-is-complete,
    );

    find-definitions :$pod, :$origin;
    write-pod "html/language/$podname.html", $pod, 'language';
}

sub process-type-source(:$pod, :$podname, :$pod-is-complete) {
    my $summary = '';
    if $pod.contents[1] ~~ {$_ ~~ Pod::Block::Named and .name eq "SUBTITLE"} {
        $summary = $pod.contents[1].contents[0].contents[0];
    } else {
        note "$podname does not have an =SUBTITLE";
    }
    my $type = $tg.types{$podname};
    my $origin = $DR.add-new(
        :kind<type>,
        :subkinds($type ?? $type.packagetype !! 'class'),
        :categories($type ?? $type.categories !! Nil),
        :$summary,
        :$pod,
        :$pod-is-complete,
        :name($podname),
    );

    find-definitions :$pod, :$origin;
}

# XXX: Generalize
sub write-type-source($doc) {
    my $pod     = $doc.pod;
    my $podname = $doc.name;
    my $type    = $tg.types{$podname};

    if !$doc.pod-is-complete {
        $pod = pod-with-title("$doc.subkinds() $podname", $pod[1..*])
    }

    if $type {
        my $tg-preamble = qq[<h1>Type graph</h1>\n<p>Below you should see
        an image showing the type relations for $podname. If not, try the <a
        href="/images/type-graph-{uri_escape $podname}.png">PNG
        version</a>.</p>];
        $pod.contents.push: Pod::Raw.new(
            target => 'html',
            contents => $tg-preamble ~ svg-for-file("html/images/type-graph-$podname.svg"),

        );

        my @mro = $type.mro;
           @mro.shift; # current type is already taken care of

        for $type.roles -> $r {
            note "No documentation seen for $r" unless %written{$r}:exists;
            next unless %methods-by-type{$r}:exists;
            $pod.contents.push:
                pod-heading("Methods supplied by role $r"),
                pod-block(
                    "$podname does role ",
                    pod-link($r.name, "/type/{uri_escape ~$r}"),
                    ", which provides the following methods:",
                ),
                %methods-by-type{$r}.list,
                ;
        }
        for @mro -> $c {
            note "No documentation seen for $c" unless %written{$c}:exists;
            next unless %methods-by-type{$c}:exists;
            $pod.contents.push:
                pod-heading("Methods supplied by class $c"),
                pod-block(
                    "$podname inherits from class ",
                    pod-link($c.name, "/type/{uri_escape ~$c}"),
                    ", which provides the following methods:",
                ),
                %methods-by-type{$c}.list,
                ;
            for $c.roles -> $r {
                note "No documentation seen for $r" unless %written{$r}:exists;
                next unless %methods-by-type{$r};
                $pod.contents.push:
                    pod-heading("Methods supplied by role $r"),
                    pod-block(
                        "$podname inherits from class ",
                        pod-link($c.name, "/type/{uri_escape ~$c}"),
                        ", which does role ",
                        pod-link($r.name, "/type/{uri_escape ~$r}"),
                        ", which provides the following methods:",
                    ),
                    %methods-by-type{$r}.list,
                    ;
            }
        }
    } else {
        note "Type $podname not found in type-graph data";
    }

    write-pod "html/type/$podname.html", $pod, 'type';
}

sub find-definitions (:$pod, :$origin, :$min-level = -1) {
    # Run through the pod content, and look for headings.
    # If a heading is a definition, like "class FooBar", process
    # the class and give the rest of the pod to find-definitions,
    # which will return how far the definition of "class FooBar" extends.
    my @c := $pod ~~ Positional ?? @$pod !! $pod.contents;
    my int $i = 0;
    my int $len = +@c;
    while $i < $len {
        NEXT {$i = $i + 1}
        my $c := @c[$i];
        next unless $c ~~ Pod::Heading;
        return $i if $c.level <= $min-level;

        # Is this new header a definition?
        # If so, begin processing it.
        # If not, skip to the next heading.
        my @header := $c.contents[0].contents;
        my @definitions; # [subkind, name]
        my $unambiguous = False;
        given @header {
            when :("", Pod::FormattingCode $, "") {
                proceed unless .[1].type eq "X";
                @definitions = .[1].meta[];
                $unambiguous = True;
            }
            when :(Str $ where /^The \s \S+ \s \w+$/) {
                # The Foo Infix
                @definitions = [.[0].words[2,1]];
            }
            when :(Str $ where {m/^(\w+) \s (\S+)$/}) {
                # Infix Foo
                @definitions = [.[0].words[0,1]];
            }
            when :("The ", Pod::FormattingCode $, Str $ where /^\s (\w+)$/) {
                # The C<Foo> infix
                @definitions = [.[2].words[0], .[1].contents[0]];
            }
            when :(Str $ where /^(\w+) \s$/, Pod::FormattingCode $, "") {
                # infix C<Foo>
                @definitions = [.[0].words[0], .[1].contents[0]];
            }
            default { next }
        }

        my int $new-i = $i;
        for @definitions -> [$sk, $name] {
            my $subkinds = $sk.lc;
            my %attr;
            given $subkinds {
                when / ^ [in | pre | post | circum | postcircum ] fix | listop / {
                    %attr = :kind<routine>,
                            :categories<operator>,
                }
                when 'sub'|'method'|'term'|'routine' {
                    %attr = :kind<routine>,
                            :categories($subkinds),
                }
                when 'class'|'role' {
                    my $summary = '';
                    if @c[$i+1] ~~ {$_ ~~ Pod::Block::Named and .name eq "SUBTITLE"} {
                        $summary = @c[$i+1].contents[0].contents[0];
                    } else {
                        note "$name does not have an =SUBTITLE";
                    }
                    %attr = :kind<type>,
                            :categories($tg.types{$name}.?categories//''),
                            :$summary,
                }
                when 'variable'|'sigil'|'twigil'|'declarator'|'quote' {
                    # TODO: More types of syntactic features
                    %attr = :kind<syntax>,
                            :categories($subkinds),
                }
                when $unambiguous {
                    # Index anything from an X<>
                    %attr = :kind<syntax>,
                            :categories($subkinds),
                }
                default {
                    # No clue, probably not meant to be indexed
                    last
                }
            }

            # We made it this far, so it's a valid definition
            my $created = $DR.add-new(
                :$origin,
                :pod[],
                :!pod-is-complete,
                :$name,
                :$subkinds,
                |%attr
            );

            # Preform sub-parse, checking for definitions elsewhere in the pod
            # And updating $i to be after the places we've already searched
            once {
                $new-i = $i + find-definitions
                    :pod(@c[$i+1..*]), :origin($created), :min-level(@c[$i].level);
            }

            my $new-head = Pod::Heading.new(
                :level(@c[$i].level),
                :contents[pod-link "$subkinds $name",
                    $created.url ~ "#$origin.human-kind() $origin.name()".subst(:g, /\s+/, '_')
                ]
            );
            my @orig-chunk = $new-head, @c[$i ^.. $new-i];
            my $chunk = $created.pod.push: pod-lower-headings(@orig-chunk, :to(%attr<kind> eq 'type' ?? 0 !! 2));
            
            if $subkinds eq 'routine' {
                # Determine proper subkinds
                my Str @subkinds = first-code-block($chunk)\
                    .match(:g, /:s ^ 'multi'? (sub|method)»/)\
                    .>>[0]>>.Str.unique;

                note "The subkinds of routine $created.name() in $origin.name() cannot be determined."
                    unless @subkinds;

                $created.subkinds   = @subkinds;
                $created.categories = @subkinds;
            }
            if $subkinds ∋ 'method' {
                %methods-by-type{$origin.name}.push: $chunk;
                write-qualified-method-call(
                    :$name,
                    :pod($chunk),
                    :type($origin.name),
                );
            }
        }
        $i = $new-i + 1;
    }
    return $i;
}

sub write-type-graph-images(:$force) {
    unless $force {
        my $dest = 'html/images/type-graph-Any.svg'.IO;
        if $dest.e && $dest.modified >= 'type-graph.txt'.IO.modified {
            say "Not writing type graph images, it seems to be up-to-date";
            say "To force writing of type graph images, supply the --typegraph";
            say "option at the command line, or delete";
            say "file 'html/images/type-graph-Any.svg'";
            return;
        }
    }
    say 'Writing type graph images to html/images/ ...';
    for $tg.sorted -> $type {
        my $viz = Perl6::TypeGraph::Viz.new-for-type($type);
        $viz.to-file("html/images/type-graph-{$type}.svg", format => 'svg');
        $viz.to-file("html/images/type-graph-{$type}.png", format => 'png', size => '8,3');
        print '.'
    }
    say '';

    say 'Writing specialized visualizations to html/images/ ...';
    my %by-group = $tg.sorted.classify(&viz-group);
    %by-group<Exception>.push: $tg.types< Exception Any Mu >;
    %by-group<Metamodel>.push: $tg.types< Any Mu >;

    for %by-group.kv -> $group, @types {
        my $viz = Perl6::TypeGraph::Viz.new(:types(@types),
                                            :dot-hints(viz-hints($group)),
                                            :rank-dir('LR'));
        $viz.to-file("html/images/type-graph-{$group}.svg", format => 'svg');
        $viz.to-file("html/images/type-graph-{$group}.png", format => 'png', size => '8,3');
    }
}

sub viz-group ($type) {
    return 'Metamodel' if $type.name ~~ /^ 'Perl6::Metamodel' /;
    return 'Exception' if $type.name ~~ /^ 'X::' /;
    return 'Any';
}

sub viz-hints ($group) {
    return '' unless $group eq 'Any';

    return '
    subgraph "cluster: Mu children" {
        rank=same;
        style=invis;
        "Any";
        "Junction";
    }
    subgraph "cluster: Pod:: top level" {
        rank=same;
        style=invis;
        "Pod::Config";
        "Pod::Block";
    }
    subgraph "cluster: Date/time handling" {
        rank=same;
        style=invis;
        "Date";
        "DateTime";
        "DateTime-local-timezone";
    }
    subgraph "cluster: Collection roles" {
        rank=same;
        style=invis;
        "Positional";
        "Associative";
        "Baggy";
    }
';
}

sub tap-search-file () {
    sub escape(Str $s) {
        $s.trans([</ \\ ">] => [<\\/ \\\\ \\">]);
    }
    ( $DR.lookup('language', :by<kind>),
      $DR.lookup('type',     :by<kind>),
      $DR.lookup('routine',  :by<kind>),
      $DR.lookup('syntax',    :by<kind>),
    ).reduce({merge $^a: $^b}).map({
        .unique(:as{.name}).map({
            .subkinds.map(*.wordcase).map: -> $subkind {
                qq[\{ label: "$subkind: {escape .name}", value: "{escape .name}", url: "{.url}" \}]
            }
        })
    }).grab({
        .reduce({merge $^a: $^b}).grab({ .join("\n") }).tap: {
            my $template = slurp("template/search_template.js");
            spurt("html/js/search.js", $template.subst("ITEMS", $_));
            say 'Wrote html/js/search.js ...';
        }
    }).tap
}

sub tap-disambiguation-files () {
    $DR.grouped-by('name').tap: {
        my ($name, $disambig) = .key, .value;
        $disambig.grab({$_}).tap: -> $p {
            my $pod = pod-with-title("Disambiguation for '$name'");
            if $p.elems == 1 {
                $p = $p[0] if $p ~~ Array;
                if $p.origin -> $o {
                    $pod.contents.push:
                        pod-block(
                            pod-link("'$name' is a $p.human-kind()", $p.url),
                            ' from ',
                            pod-link($o.human-kind() ~ ' ' ~ $o.name, $o.url),
                        );
                }
                else {
                    $pod.contents.push:
                        pod-block(
                            pod-link("'$name' is a $p.human-kind()", $p.url)
                        );
                }
            }
            else {
                $pod.contents.push:
                    pod-block("'$name' can be anything of the following"),
                    $p.map({
                        if .origin -> $o {
                            pod-item(
                                pod-link(.human-kind, .url),
                                ' from ',
                                pod-link($o.human-kind() ~ ' ' ~ $o.name, $o.url),
                            )
                        }
                        else {
                            pod-item( pod-link(.human-kind, .url) )
                        }
                    });
            }
            my $html = p2h($pod, 'routine');
            write-pod "html/$name.subst(/<[/\\]>/,'_',:g).html", $html;
        }
    }
}

sub tap-index-files () {
    write-pod 'html/index.html', EVAL slurp('lib/HomePage.pod') ~ "\n\$=pod";

    $DR.lookup('language', :by<kind>).tap: {
        .sort({.name}).map({[
            pod-link(.name, .url),
            .summary
        ]}).grab({
            pod-with-title(
                "Perl 6 Language Documentation",
                pod-table $_,
            )
        }).tap: {
            write-pod "html/language.html", $_, 'language';
        }
    }

    my &preamble = { pod-block
        'This is a list of ', pod-bold('all'), ' built-in ' ~ $^kind.tc ~
        "s that are documented here as part of the Perl 6 language. " ~
        "Use the above menu to narrow it down topically."
    }
    my $kind = 'type';
    $DR.lookup($kind, :by<kind>).tap: -> $k {
        tap-index $k, :$kind, :&preamble;
        for <basic composite domain-specific exceptions> -> $category {
            tap-index $k.grep({.categories.any eq $category}), :$kind, :$category;
        }
    }

    my &summary = { 
        .map({ pod-link .origin.name, .origin.url })\
        .grab({.reduce({$^a,", ",$^b})}).tap: {
            pod-block "(From ", @$_, ")"
        }
    }

    $kind = 'routine';
    $DR.lookup('routine', :by<kind>).tap: -> $r {
        tap-index $r, :$kind, :&summary, :&preamble;
        for <sub method term operator> -> $category {
            tap-index $r.grep({.categories.any eq $category}), :$kind, :&summary, :$category;
        }
    }
}

sub tap-index(Supply $s, :$kind!, :$category = Nil, :&summary = {Nil}, :&preamble) {
    $s.categorize({.name}).map(-> $p {
        $p.value.grab(-> @v {
            $p.key => [
                say @v.elems;
              @v.map({.subkinds // Nil}).unique.join(', '),
              pod-link(@v[0].name, @v[0].url),
              @v.&summary,
            ]
        })
    }).reduce(-> $a, *@b { merge $a: @b }).tap: {
        my $t = .grab({ pod-with-title
                "Perl 6 {join " ", $category.tc, $kind.tc}s",
                preamble($kind),
                pod-table .sort({.key}).map({.value}),
        }).tap: {
            write-pod "html/{join "-", $kind, $category//Nil}.html", $_, $kind;
        }
    }
}

sub tap-kind ($kind) {
    $DR.lookup($kind, :by<kind>).tap: {
        .categorize({.name}).tap: {
            my ($name, $d-supply) = .key, .value;
            my @subkinds = $d-supply.map({.subkinds}).unique;
            $d-supply.map({
                pod-heading("{.origin.human-kind} {.origin.name}"),
                pod-block("From ",
                    pod-link(.origin.name, .origin.url ~ '#' ~ (.subkinds~'_' if .subkinds ~~ /fix/) ~ .name)
                ),
                .pod.list,
            }).grab({
                my $subkind = @subkinds.elems == 1 ?? @subkinds.list[0] !! $kind;
                pod-with-title
                    "Documentation for $subkind $name",
                    pod-block("Documentation for $subkind $name, assembled from the following types:"),
                    @$_
            }).tap: {
                write-pod "html/$kind/$name.subst(/<[/\\]>/,'_',:g).html", $_, $kind;
            }
        }
    }
}

sub write-qualified-method-call(:$name!, :$pod!, :$type!) {
    my $p = pod-with-title(
        "Documentation for method $type.$name",
        pod-block('From ', pod-link($type, "/type/{$type}#$name")),
        @$pod,
    );
    write-pod "html/routine/{$type}.{$name}.html", $p, 'routine';
}

sub footer-html() {
    state $dt = ~DateTime.now;
    my $footer = slurp 'template/footer_template.html';
    my $footer_content = qq[
        <p>
            Generated on $dt from the sources at
            <a href="https://github.com/perl6/doc">perl6/doc on github</a>.
            This is a work in progress to document Perl 6, and known to be
            incomplete. Your contribution is appreciated.
        </p>
        <p>
            This documentation is provided under the terms of the Artistic
            License 2.0.
            The Camelia image is copyright 2009 by Larry Wall.
        </p>
    ];
    $footer.subst('CONTENT', $footer_content);
}
