use v6;
use Perl6::Documentable;
use OO::Monitors;

monitor Perl6::Documentable::Registry {
    has $.documentables = Supply.new;
    has Bool $.composed = False;
    has %!cache;
    has %!grouped-by;
    has %!lookup-cache;
    method add-new(*%args) {
        die "Cannot add something to a composed registry" if $!composed;
        $!documentables.emit: my $d = Perl6::Documentable.new(|%args);
        $d;
    }
    method compose() {
        $!documentables.done;
        $!composed = True;
    }
    method grouped-by(Str $what --> Supply) {
        %!grouped-by{$what} //= $!documentables.classify({$_."$what"()});
    }
    method lookup(Str $what, Str :$by! --> Supply) {
        $.grouped-by($by).grep({.key eq $what}).map({.value})
    }
}
