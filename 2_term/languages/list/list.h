#include <string>

struct list
{
private:
	struct node
	{
		std::string str;
		node* prev;
		node* next;
		node(std::string const& _str = "",
				node* _prev = nullptr, node* _next = nullptr);
	};
	node* head;
	node* tail;

public:
	list();
	list(list const &);
	~list();

	void swap(list& other);
	
	list& operator=(list);

	void push_back(std::string const&);
	void push_front(std::string const&);
	void pop_back();
	void pop_front();

	std::string& front();
	std::string const& front() const;
	std::string& back();
	std::string const& back() const;

	bool empty() const;

	struct iterator
	{
		iterator& operator++();
		iterator operator++(int);
		iterator& operator--();
		iterator operator--(int);

		std::string& operator*() const;

		bool operator==(iterator);
		bool operator!=(iterator);

		friend struct list;

	private:
		node* v;

		iterator(node*);
	};

	iterator begin();
	iterator end();

	iterator insert(iterator, std::string const&);
	iterator erase(iterator);
	iterator slice(iterator pos, iterator first, iterator last);

	struct const_iterator
	{
		const_iterator& operator++();
		const_iterator operator++(int);
		const_iterator& operator--();
		const_iterator operator--(int);

		std::string const& operator*() const;

		bool operator==(const_iterator);
		bool operator!=(const_iterator);

		friend struct list;

	private:
		node* v;

		const_iterator(node*);
	};

	const_iterator begin() const;
	const_iterator end() const;
	const_iterator cbegin() const;
	const_iterator cend() const;
};
