while (<>)
{
	s/([a-zA-Z])\g1*/$1/g;
	print;
}
