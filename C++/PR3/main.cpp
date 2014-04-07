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

    void increment()
    {
        currentPos_ = stream_->tellg();
        getline(*stream_, str_);
    }

public:
    FileIterator(istream *stream, bool end_iterator = true) : str_(""), stream_(stream), currentPos_(-1)
    {
        if (!end_iterator)
            increment();
    }

    FileIterator(FileIterator const &fit)
        : str_(fit.str_), stream_(fit.stream_), currentPos_(fit.currentPos_)
    {
    }

    FileIterator &operator++()
    {
        increment();
        return *this;
    }

    FileIterator operator++(int)
    {
        FileIterator tmp(*this);
        increment();
        return tmp;
    }

    friend bool operator==(FileIterator const &a, FileIterator const &b);

    string const &operator*() const
    {
        return str_;
    }

    string const *operator->() const
    {
        return &str_;
    }
};

bool operator==(FileIterator const &a, FileIterator const &b)
{
    return a.currentPos_ == b.currentPos_;
}

bool operator!=(FileIterator const &a, FileIterator const &b)
{
    return !(a == b);
}

struct FileReader {
    FileReader(istream *stream) : begin_(FileIterator(stream, false)), end_(FileIterator(stream))
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
