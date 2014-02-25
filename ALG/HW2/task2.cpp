#include <iostream>
#include <vector>
#include <algorithm>
#include <vector>
#include <cstdio>

using namespace std;

class AVLTree {
public:
    AVLTree() : root(NULL)
    {
    }

    void insert(int key)
    {
        root = insert(root, key);
    }

    int realNumber(int key)
    {
        return realNumber(root, 0, key);
    }

private:
    struct node {
        int key;
        int height;
        int size;
        node *left;
        node *right;

        node(int k) : key(k), height(1), size(1), left(NULL), right(NULL)
        {
        }
    };

    node *root;

    int realNumber(node *p, int left, int n)
    {
        if (!p)
            return n + left;

        int free = p->key - size(p->left) - 1 - left;

        if (free >= n)
            return realNumber(p->left, left, n);
        else
            return realNumber(p->right, p->key, n - free);
    }

    node *insert(node *p, int k)
    {
        if (!p)
            return new node(k);

        if (k < p->key) {
            p->left = insert(p->left, k);
        } else {
            p->right = insert(p->right, k);
        }
        p->size++;
        return rebalance(p);
    }

    int height(node *p)
    {
        return p ? p->height : 0;
    }

    int size(node *p)
    {
        return p ? p->size : 0;
    }

    int disbalance(node *p)
    {
        return height(p->right) - height(p->left);
    }

    void update_height(node *p)
    {
        int l = height(p->left);
        int r = height(p->right);
        p->height = max(l, r) + 1;
    }

    void update(node *p, node *q)
    {
        update_height(p);
        update_height(q);

        q->size = p->size;
        p->size = size(p->left) + size(p->right) + 1;
    }

    node *rotate_right(node *p)
    {
        node *q = p->left;

        p->left = q->right;
        q->right = p;

        update(p, q);
        return q;
    }

    node *rotate_left(node *p)
    {
        node *q = p->right;

        p->right = q->left;
        q->left = p;

        update(p, q);
        return q;
    }

    node *rebalance(node *p)
    {
        update_height(p);

        if (disbalance(p) == 2) {
            if (disbalance(p->right) < 0)
                p->right = rotate_right(p->right);
            return rotate_left(p);
        } else if (disbalance(p) == -2) {
            if (disbalance(p->left) > 0)
                p->left = rotate_left(p->left);
            return rotate_right(p);
        }

        return p;
    }
};

AVLTree tree;

int main()
{
    // freopen("input.txt", "r", stdin);
    
    int n, m, x;
    char c;
    cout.sync_with_stdio(false);
    cin.sync_with_stdio(false);
    
    cin >> n >> m;

    for (int i = 0; i < m; ++i) {
        cin >> c >> x;
        int r = tree.realNumber(x);
        if (c == 'D')
            tree.insert(r);
        else
            cout << r << endl;
    }
    return 0;
}
