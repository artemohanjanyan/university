while (<>)
{
	s/\([^\)]*\)/\(\)/g;
	print;
}