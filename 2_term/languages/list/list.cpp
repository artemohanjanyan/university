#include "list.h"
#include <exception>

list::list() :
		head(new node()), tail(new node())
{
	head->next = tail;
	tail->prev = head;
}

// TODO: check if it is exception-safe
list::list(list const& other) :
		list()
{
	try
	{
		for (auto& str : other)
			push_back(str);
	}
	catch (std::exception& e)
	{
		this->~list();
		throw e;
	}
}

list::~list()
{
	while (!empty())
		pop_back();
	delete head;
	delete tail;
}

void list::swap(list& other)
{
	std::swap(head, other.head);
	std::swap(tail, other.tail);
}

list& list::operator=(list other)
{
	swap(other);
	return *this;
}

void list::push_back(std::string const& str)
{
	insert(end(), str);
}

void list::push_front(std::string const& str)
{
	insert(begin(), str);
}

void list::pop_back()
{
	erase(--end());
}

void list::pop_front()
{
	erase(begin());
}

std::string& list::front()
{
	return *begin();
}

std::string const& list::front() const
{
	return *begin();
}

std::string& list::back()
{
	return *(--end());
}

std::string const& list::back() const
{
	return *(--end());
}

bool list::empty() const
{
	return head->next == tail;
}

/**
 * Node
 */

list::node::node(std::string const& _str, node* _prev, node* _next) :
		str(_str), prev(_prev), next(_next)
{}

/**
 * Iterator
 */

list::iterator::iterator(node* _v) : v(_v)
{}

list::iterator& list::iterator::operator++()
{
	v = v->next;
	return *this;
}

list::iterator list::iterator::operator++(int)
{
	list::iterator tmp = *this;
	++*this;
	return tmp;
}

list::iterator& list::iterator::operator--()
{
	v = v->prev;
	return *this;
}

list::iterator list::iterator::operator--(int)
{
	list::iterator tmp = *this;
	--*this;
	return tmp;
}

std::string& list::iterator::operator*() const
{
	return v->str;
}

bool list::iterator::operator==(list::iterator other)
{
	return v == other.v;
}

bool list::iterator::operator!=(list::iterator other)
{
	return !(*this == other);
}

/**
 * Const iterator
 */

list::const_iterator::const_iterator(node* _v) : v(_v)
{}

list::const_iterator& list::const_iterator::operator++()
{
	v = v->next;
	return *this;
}

list::const_iterator list::const_iterator::operator++(int)
{
	list::const_iterator tmp = *this;
	++*this;
	return tmp;
}

list::const_iterator& list::const_iterator::operator--()
{
	v = v->prev;
	return *this;
}

list::const_iterator list::const_iterator::operator--(int)
{
	list::const_iterator tmp = *this;
	--*this;
	return tmp;
}

std::string const& list::const_iterator::operator*() const
{
	return v->str;
}

bool list::const_iterator::operator==(list::const_iterator other)
{
	return v == other.v;
}

bool list::const_iterator::operator!=(list::const_iterator other)
{
	return !(*this == other);
}

/**
 * Iterator operations
 */

list::iterator list::begin()
{
	return list::iterator(head->next);
}

list::iterator list::end()
{
	return list::iterator(tail);
}

list::const_iterator list::begin() const
{
	return list::const_iterator(head->next);
}

list::const_iterator list::end() const
{
	return list::const_iterator(tail);
}

list::const_iterator list::cbegin() const
{
	return begin();
}

list::const_iterator list::cend() const
{
	return end();
}

list::iterator list::insert(list::iterator it, std::string const& str)
{
	node* tmp = new node(str, it.v->prev, it.v);
	it.v->prev = it.v->prev->next = tmp;
	return list::iterator(tmp);
}

list::iterator list::erase(list::iterator it)
{
	node* next = it.v->next;

	it.v->next->prev = it.v->prev;
	it.v->prev->next = it.v->next;
	delete it.v;

	return iterator(next);
}

list::iterator list::slice(list::iterator pos, list::iterator first, list::iterator last)
{
	// Delete
	first.v->prev->next = last.v;
	--last;
	last.v->next->prev = first.v->prev;

	// Range -> pos
	first.v->prev = pos.v->prev;
	last.v->next = pos.v;

	// pos -> Range
	pos.v->prev->next = first.v;
	pos.v->prev = last.v;

	return first;
}
