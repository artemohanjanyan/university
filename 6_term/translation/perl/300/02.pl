$q = 1;
$prev = 'qq';

while (<>)
{
	s/<.*?>//g;

	if ($q == 1)
	{
		if (!/^\s*$/)
		{
			$q = 2;
		}
	}

	if ($q == 2)
	{
		if (!/^\s*$/)
		{
			if ($prev =~ /^$/)
			{
				print $prev;
				$prev = 'qq';
			}
			s/^\s+//g;
			s/\s+$//g;
			s/(\s)\s+/$1/g;
			print $_ . "\n";
		}
		else
		{
			$prev = "\n";
		}
	}
}
