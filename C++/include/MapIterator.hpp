/**
 * Borrowed from http://john-ahlgren.blogspot.com/2013/10/how-to-iterate-over-values-of-stdmap.html
 */

#include <map>
#include <iterator>

namespace MNRL {
	template <typename Iter>
	class MapIterator : public std::iterator<std::bidirectional_iterator_tag,typename Iter::value_type::second_type> {
	public:
		MapIterator() {}
		MapIterator(Iter j) : i(j) {}
		MapIterator& operator++() { ++i; return *this; }
		MapIterator operator++(int) { auto tmp = *this; ++(*this); return tmp; }
		MapIterator& operator--() { --i; return *this; }
		MapIterator operator--(int) { auto tmp = *this; --(*this); return tmp; }
		bool operator==(MapIterator j) const { return i == j.i; }
		bool operator!=(MapIterator j) const { return !(*this == j); }
		typename std::iterator<std::bidirectional_iterator_tag, typename Iter::value_type::second_type>::reference operator*() { return i->second; }
		typename std::iterator<std::bidirectional_iterator_tag, typename Iter::value_type::second_type>::pointer operator->() { return &i->second; }

		std::iterator<std::bidirectional_iterator_tag,typename Iter::value_type::second_type> begin() { return i.begin(); }
		std::iterator<std::bidirectional_iterator_tag,typename Iter::value_type::second_type> end() { return i.end(); }
	protected:
		Iter i;
	};

}

