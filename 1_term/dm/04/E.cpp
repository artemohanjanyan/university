#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include <cassert>
using namespace std;
typedef long long LL;

const double eps = 1e-10;

struct Matrix
{
	int height, width;
	double **matrix, *arrayPointer;

	//Constructors

	Matrix() : height(0), width(0), matrix(nullptr), arrayPointer(nullptr)
	{}

	Matrix(int height, int width) : height(height), width(width), matrix(new double*[height])
	{
		arrayPointer = matrix[0] = new double[height * width];
		for (int i = 1; i < height; ++i)
			matrix[i] = matrix[i - 1] + width;

		for (int i = 0; i < height; ++i)
			for (int j = 0; j < width; ++j)
				matrix[i][j] = 0;
	}

	Matrix(const Matrix &that) : height(that.height), width(that.width), matrix(new double*[height])
	{
		arrayPointer = matrix[0] = new double[height * width];
		for (int i = 1; i < height; ++i)
			matrix[i] = matrix[i - 1] + width;

		for (int i = 0; i < height; ++i)
			for (int j = 0; j < width; ++j)
				matrix[i][j] = that[i][j];
	}

	//Move

	Matrix& operator=(Matrix &&that)
	{
		swap(height, that.height);
		swap(width, that.width);
		swap(matrix, that.matrix);
		swap(arrayPointer, that.arrayPointer);

		return *this;
	}

	Matrix(Matrix &&that) : height(0), width(0), matrix(nullptr), arrayPointer(nullptr)
	{
		*this = forward<Matrix>(that);
	}

	// Destructor

	~Matrix()
	{
		delete[] arrayPointer;
		delete[] matrix;
	}

	//Matrix access

	double const* operator[](int i) const
	{
		return matrix[i];
	}

	double* operator[](int i)
	{
		return matrix[i];
	}

	//Operations

	Matrix& operator*=(const Matrix &that);
	Matrix& operator*=(double x);

	Matrix& operator+=(const Matrix &that);
	Matrix& operator-=(const Matrix &that);

	Matrix reverse();
};

Matrix operator*(const Matrix &a, const Matrix &b)
{
	assert(a.width == b.height);
	Matrix ans(a.height, b.width);

	for (int i = 0; i < a.height; ++i)
		for (int j = 0; j < b.width; ++j)
			for (int k = 0; k < a.width; ++k)
				ans[i][j] += a[i][k] * b[k][j];

	return ans;
}

Matrix& Matrix::operator*=(const Matrix &that)
{
	return *this = *this * that;
}

Matrix& Matrix::operator*=(double x)
{
	for (int i = 0; i < height; ++i)
		for (int j = 0; j < width; ++j)
			matrix[i][j] *= x;

	return *this;
}

Matrix operator*(Matrix a, double x)
{
	return a *= x;
}

Matrix& Matrix::operator+=(const Matrix &that)
{
	for (int i = 0; i < height; ++i)
		for (int j = 0; j < width; ++j)
			matrix[i][j] += that[i][j];

	return *this;
}

Matrix operator+(Matrix a, const Matrix &b)
{
	return a += b;
}

Matrix& Matrix::operator-=(const Matrix &that)
{
	for (int i = 0; i < height; ++i)
		for (int j = 0; j < width; ++j)
			matrix[i][j] -= that[i][j];

	return *this;
}

Matrix operator-(Matrix a, const Matrix &b)
{
	return a -= b;
}

ostream& operator<<(ostream &out, const Matrix &matrix)
{
	for (int i = 0; i < matrix.height; ++i)
	{
		for (int j = 0; j < matrix.width; ++j)
			out << matrix[i][j] << " ";
		out << "\n";
	}

	return out;
}

Matrix Matrix::reverse()
{
	assert(height == width);
	int n = height;

	Matrix cur(*this), rev(n, n);
	for (int i = 0; i < n; ++i)
		for (int j = 0; j < n; ++j)
			if (i != j)
				rev[i][j] = 0;
			else
				rev[i][j] = 1;

	for (int i = 0; i < n; ++i)
	{
		if (abs(cur[i][i]) < eps)
			for (int j = i + 1; j < n; ++j)
				if (abs(cur[j][i]) > eps)
				{
					for (int k = 0; k < n; ++k)
					{
						//cur[i][k] += cur[j][k];
						//rev[i][k] += rev[j][k];
						swap(cur[i][k], cur[j][k]);
						swap(rev[i][k], rev[j][k]);
					}
					break;
				}

		for (int j = i + 1; j < n; ++j)
			if (abs(cur[j][i]) > eps)
			{
				double koef = -cur[j][i] / cur[i][i];
				for (int k = 0; k < n; ++k)
				{
					cur[j][k] += koef * cur[i][k];
					rev[j][k] += koef * rev[i][k];
				}
			}
	}

	for (int i = n - 1; i >= 0; --i)
	{
		for (int j = i - 1; j >= 0; --j)
			if (abs(cur[j][i]) > eps)
			{
				double koef = -cur[j][i] / cur[i][i];
				for (int k = 0; k < n; ++k)
				{
					cur[j][k] += koef * cur[i][k];
					rev[j][k] += koef * rev[i][k];
				}
			}
	}

	for (int i = 0; i < n; ++i)
	{
		double koef = 1 / cur[i][i];
		for (int j = 0; j < n; ++j)
		{
			cur[i][j] *= koef;
			rev[i][j] *= koef;
		}
	}

	return rev;
}

int main()
{
	ifstream cin("markchain.in");
	ofstream cout("markchain.out");

	int n;
	cin >> n;
	Matrix p(n, n);
	for (int i = 0; i < n; ++i)
		for (int j = 0; j < n; ++j)
			cin >> p[i][j];

	for (int i = 0; i < 20; ++i)
		p *= p;

	Matrix start(1, n);
	for (int i = 0; i < n; ++i)
		start[0][i] = 1.0 / n;
	
	Matrix ans = start * p;

	cout.precision(9);
	cout << fixed;
	for (int i = 0; i < n; ++i)
		cout << ans[0][i] << "\n";

	return 0;
}
