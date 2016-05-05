const
	maxn = 1001;

var
	a : array [1..1001, 1..1001] of char;
	i, j, n : integer;

function compare(ai, bi : integer) : boolean;
var
	i : integer;
begin
	for i := 1 to n do
		if (a[ai, i] <> a[bi, i]) then
		begin
			compare := a[ai, i] < a[bi, i];
			exit;
		end;
	compare := false;
end;

procedure swap(ai, bi : integer);
var
	i : integer;
	c : char;
begin
	for i := 1 to n do
	begin
		c := a[ai, i];
		a[ai, i] := a[bi, i];
		a[bi, i] := c;
	end;
end;

procedure sort(l, r : integer);
var
	mid, i, j : integer;
begin
	mid := random(r - l + 1) + l;
	for i := 1 to n do
		a[maxn, i] := a[mid, i];
	i := l;
	j := r;

	while (i < j) do
	begin
		while (compare(i, maxn)) do
			inc(i);
		while (compare(maxn, j)) do
			dec(j);

		if (i <= j) then
		begin
			swap(i, j);
			inc(i);
			dec(j);
		end;
	end;

	if (l < j) then
		sort(l, j);
	if (i < r) then
		sort(i, r);
end;

begin
	assign(input, 'bwt.in');
	reset(input);
	assign(output, 'bwt.out');
	rewrite(output);

	n := 1;
	while (not eoln) do
	begin
		read(a[1, n]);
		inc(n);
	end;
	dec(n);

	for i := 2 to n do
		for j := 1 to n do
			a[i, j] := a[1, (i + j - 2) mod n + 1];

	sort(1, n);
	
	for i := 1 to n do
		write(a[i, n]);
end.
