while (<>)
{
	s/\([^\)]*\)/\(\)/g;
	print;
}
