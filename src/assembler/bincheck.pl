#!/usr/bin/env perl
use strict;
use warnings;
use 5.010;

use Data::Dumper;
use Data::ParseBinary;

sub Ber {
	my ($name) = @_;
	Struct(
		$name,
		RepeatUntil(sub {($_->obj & 0x80) != 0x80}, Byte("cont")),
		Value("value", sub {
			my @cont = @{$_->ctx->{cont}};
			my $value = 0;
			while (@cont) { $value = ($value << 7) | (shift @cont & 0x7f); }
			return $value;
		}));
}

sub Int {
	my ($name) = @_;
	Struct(
		$name,
		Ber("ber"),
		Value("value", sub {
			my $b = $_->ctx->{ber}->{value};
			if ($b & 0x01) {
				return -($b >> 1) - 1;
			}
			return ($b >> 1);
		}));
}

sub Str {
	my ($name) = @_;
	Struct(
		$name,
		Ber("len"),
		Array(sub { $_->ctx->{len}->{value}; }, Byte("array")),
		Value("string", sub { join("", map { chr; } @{$_->ctx->{array}}) }));
}

sub BClass {
	Struct(
		"class",
		Str("id"),
		Str("parent"),
		Ber("slot_num"),
		Array(sub { $_->ctx->{slot_num}->{value}; }, Str("slot_ids")));
}

sub BConst {
	my ($name) = @_;
	Struct(
		$name,
		Enum(Byte("type"),
			INT => 0,
                        CHAR => 1,
			STRING => 2,
		),
		Switch("data", sub { $_->ctx->{type}; }, {
			INT => Int("value"),
                        CHAR => UBInt32("value"),
			STRING => Str("value"),
		}));
}

sub BFunc {
	Struct(
		"func",
		Str("id"),
		Byte("variable_arg"),
		Ber("arg_num"),
		Array(sub { $_->ctx->{arg_num}->{value}; }, Str("arg_klass")),
		Ber("func_slot_num"),
		Ber("var_slot_num"),
		Ber("stack_size"),
		Ber("ctable_size"),
		Array(sub { $_->ctx->{ctable_size}->{value}; }, BConst("ctable")),
		Ber("bcode_size"),
		Array(sub { $_->ctx->{bcode_size}->{value}; }, UBInt32("bcode")));
}

sub BObj {
	Struct(
		"obj",
		Enum(Byte("type"),
			CLASS => 0,
			FUNC => 1,
		),
		Switch("data", sub { $_->ctx->{type}; }, {
			CLASS => BClass(),
			FUNC => BFunc(),
		}));
}

sub BFile {
	Struct(
		"file",
		Ber("item_num"),
		Array(sub { $_->ctx->{item_num}->{value}; }, BObj()));
}

my $FH;
open $FH, "<$ARGV[0]" or die "File not found: $ARGV[0]";
binmode $FH;

print Dumper BFile()->parse(CreateStreamReader(File => $FH));

close $FH;

