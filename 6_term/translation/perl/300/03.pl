$file = "";
foreach $line (<>)
{
	$file = $file . $line;
}
$file =~ s/\R/ /g;

%sites = ();

while ($file =~ /(<a(.+?)\bhref\s*=\s*('\s*(.*?)\s*'|"\s*(.*?)\s*")(.*?)>)/)
{
	$str = $4 . $5;
	$str =~ s/^.*?:\/\///;
	$str =~ s/[:\/?#].*$//;

	if (!($str =~ /^$/) && !($str =~ /^\./) && !($str =~ /\.$/) && !($str =~ /\.\./))
	{
		$sites{$str} = 1;
	}

	$file =~ s/<a.*?>//;
}

foreach $site (sort(keys %sites))
{
	print $site . "\n";
}
