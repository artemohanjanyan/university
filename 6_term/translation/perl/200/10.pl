while (<>)
{
	s/a(([^a]|a(?!a))*aa){2}[^a]*a/bad/g;
	print;
}
