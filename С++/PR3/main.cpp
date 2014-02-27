#include <iostream>
#include <istream>
#include <fstream>
#include <string>

using std::string;
using std::istream;
using std::ifstream;
using std::cout;
using std::endl;

class FileIterator : public std::iterator<std::input_iterator_tag, string> {
private:
    string str_;
    istream *stream_;
    long currentPos_;

public:
    FileIterator(istream *stream, bool ate = false) : str_(""), stream_(stream), currentPos_(-1)
    {
        if (!ate)
            operator++();
    }

    FileIterator(FileIterator const &fit)
        : str_(fit.str_), stream_(fit.stream_), currentPos_(fit.currentPos_)
    {
    }

    FileIterator &operator=(FileIterator const &fit)
    {
        if (this != &fit)
            FileIterator(fit).swap(*this);
        return *this;
    }

    FileIterator &operator++()
    {
        currentPos_ = stream_->tellg();
        getline(*stream_, str_);

        return *this;
    }

    FileIterator operator++(int)
    {
        FileIterator tmp(*this);
        operator++();
        return tmp;
    }

    bool operator==(FileIterator const &f) const
    {
        return currentPos_ == f.currentPos_;
    }

    bool operator!=(FileIterator const &f) const
    {
        return currentPos_ != f.currentPos_;
    }

    string const & operator*() const
    {
        return str_;
    }

    string const * operator->() const
    {
        return &(*(*this));
    }

    void swap(FileIterator &f)
    {
        std::swap(str_, f.str_);
        std::swap(stream_, f.stream_);
        std::swap(currentPos_, f.currentPos_);
    }
};

struct FileReader {
    FileReader(istream *stream) : begin_(FileIterator(stream)), end_(FileIterator(stream, true))
    {
    }

    FileIterator begin()
    {
        return begin_;
    }

    FileIterator end()
    {
        return end_;
    }

private:
    FileIterator begin_;
    FileIterator end_;
};

FileReader getlines(istream &stream)
{
    return FileReader(&stream);
}

int main()
{
    ifstream input("main.cpp", ifstream::in);

    for (string v : getlines(input)) {
        std::cout << v << std::endl;
    }
    return 0;
}
