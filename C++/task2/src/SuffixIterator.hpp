#ifndef SUFFIXITERATOR_HPP
#define SUFFIXITERATOR_HPP

#include <boost/iterator/iterator_categories.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <string>
#include <utility>
#include "Utilities.hpp"

#include <iostream>

using std::string;

class SuffixIterator
    : public boost::iterator_facade<SuffixIterator, suffix, boost::single_pass_traversal_tag> {

private:
    suffix *current_suffix;

public:
    SuffixIterator() : current_suffix(new suffix{"", 0})
    {
        // creates the "end" iterator
    }

    ~SuffixIterator()
    {
        delete current_suffix;
    }

    explicit SuffixIterator(string const &s, size_t position_in_db)
        : current_suffix(new suffix{s, position_in_db})
    {
    }

    boost::iterator_facade<SuffixIterator, suffix, boost::single_pass_traversal_tag>::reference
    dereference() const
    {
        return *current_suffix;
    }

    bool equal(SuffixIterator const &rhs) const
    {
        return current_suffix->suff == rhs.current_suffix->suff;
    }

    SuffixIterator &increment()
    {
        if (!current_suffix->suff.empty())
            current_suffix->suff = current_suffix->suff.substr(1);
        return *this;
    }
};

#endif /* end of include guard: SUFFIXITERATOR_HPP */
