#include <iostream>
#include <vector>
#include <algorithm>

void p(int i)
{
    std::cout << i << " ";
}

void p_v(std::vector<int>& v)
{
    std::for_each(v.begin(), v.end(), p);
    std::cout << std::endl;
}

bool ifFunc(int i)
{
    return i == 80;
}

bool ifSearchFunc(int i1, int i2)
{
    return (i1 == i2);
}

int main()
{
    int a[] = {10, 20, 30, 40, 20, 20, 80};

    std::vector<int> v(a, a+sizeof(a)/sizeof(a[0]));

    p_v(v);

    // 1. 排序
    std::reverse(v.begin(), v.end());

    p_v(v);

    // 2. 查找
    // 2.1 find
    int ele = 80;
    auto iter = std::find(v.begin(), v.end(), ele);
    if (iter == v.end()) {
        std::cout << "element '" << ele << "' NOT found!" << std::endl;
    } else {
        std::cout << "element '" << ele << "' FOUND! [" << *iter << "]" << std::endl;
    }

    // 2.2 find_if
    iter = std::find_if(v.begin(), v.end(), ifFunc);
    if (iter == v.end()) {
        std::cout << "No satisifed element by find_if" << std::endl;
    } else {
        std::cout << "Find satisifed element by find_if:" << *iter << std::endl;
    }

    // 3. 统计
    // 3.1 count
    int cnt = std::count(v.begin(), v.end(), 20);
    std::cout << "20 occurs " << cnt << " times" << std::endl;

    // 3.2 count_if
    cnt = std::count_if(v.begin(), v.end(), ifFunc);
    std::cout << "80 occurs " << cnt << " times" << std::endl;

    // 4. 查找
    // search
    int a2[] = {40, 30};        
    std::vector<int> v2(a2, a2+sizeof(a2)/sizeof(a2[0]));
    iter = std::search(v.begin(), v.end(), v2.begin(), v2.end());
    if (iter == v.end()) {
        std::cout << "search v2 in v failed." << std::endl;
    } else {
        std::cout << "search v2 in v succ. At pos: " << (iter-v.begin()) << std::endl;
    }

    iter = std::search(v.begin(), v.end(), v2.begin(), v2.end(), ifSearchFunc);
    if (iter == v.end()) {
        std::cout << "search v2 in v failed." << std::endl;
    } else {
        std::cout << "search v2 in v succ. At pos: " << (iter-v.begin()) << std::endl;
    }

    // search_n
    iter = std::search_n(v.begin(), v.end(), 2, 20);
    if (iter == v.end()) {
        std::cout << "search two 20s in v failed." << std::endl;
    } else {
        std::cout << "search two 20s in v succ. At pos: " << (iter-v.begin()) << std::endl;
    }

    iter = std::search_n(v.begin(), v.end(), 2, 20, ifSearchFunc);
    if (iter == v.end()) {
        std::cout << "search two 20s in v failed." << std::endl;
    } else {
        std::cout << "search two 20s in v succ. At pos: " << (iter-v.begin()) << std::endl;
    }


    // 5. 查找相连
    // adjacent_find
    iter = std::adjacent_find(v.begin(), v.end());
    if (iter == v.end()) {
        std::cout << "search adjacent elements in v failed." << std::endl;
    } else {
        std::cout << "search adjacent elements in v succ. It's: " << *iter << std::endl;
    }

    // adjacent_find 用自定义函数
    iter = std::adjacent_find(v.begin(), v.end(), ifSearchFunc);
    if (iter == v.end()) {
        std::cout << "search adjacent elements in v failed." << std::endl;
    } else {
        std::cout << "search adjacent elements in v succ. It's: " << *iter << std::endl;
    }

    // 6. 区间查找
    // lower_bound/upper_bound
    // 在有序区间查找第一个不小/大于给定元素的值
    std::sort(v.begin(), v.end());
    p_v(v);
    auto low = std::lower_bound(v.begin(), v.end(), 20);
    auto up = std::upper_bound(v.begin(), v.end(), 20);
    std::cout << "lower_bound at pos: " << (low - v.begin()) << std::endl;
    std::cout << "upper_bound at pos: " << (up - v.begin()) << std::endl;

    // equal_range. 同时找到上下区间
    auto bounds = std::equal_range(v.begin(), v.end(), 20);
    std::cout << "bound at [" 
              << (bounds.first - v.begin()) 
              << ", " 
              << (bounds.second - v.begin()) 
              << "]" << std::endl;

    // binary_search. 返回bool值
    if (std::binary_search(v.begin(), v.end(), ele)) {
        std::cout << "find " << ele << " in v.\n";
    } else {
        std::cout << "NOT find " << ele << " in v.\n";
    }

    // min_element/max_element
    std::cout << "Min element: " << *std::min_element(v.begin(), v.end()) << std::endl;
    std::cout << "Max element: " << *std::max_element(v.begin(), v.end()) << std::endl;

    // find_end. search查找子区间第一次出现的位置，而find_end用来查找区间最后一次出现的位置
    iter = std::find_end(v.begin(), v.end(), v2.begin(), v2.end());
    if (iter == v.end()) {
        std::cout << "v2 last found at pos: " << (iter - v.begin()) << std::endl;
    }

    // 比较
    // equal. 返回bool值
    if (std::equal(v.begin(), v.end(), v2.begin())) {
        std::cout << "v equals v2" << std::endl;
    } else {
        std::cout << "v not equals v2" << std::endl;
    }

    // mismatch. 返回pair，分别指向两个序列中不一样的位置
    auto diff = std::mismatch(v.begin(), v.end(), v2.begin());
    std::cout << "First mismatching elements: " << *diff.first
              << " and " << *diff.second << std::endl;

    // 合并
    // inplace_merge. 合并两个有序序列，注意first,middle,last(第1,2,3个参数)都是在同一个容器，只是被分开了两个区间
    std::sort(v2.begin(), v2.end());
    std::vector<int> v3(9);
    iter = std::copy(v.begin(), v.end(), v3.begin());
    std::copy(v2.begin(), v2.end(), iter);
    std::inplace_merge(v3.begin(), v3.begin()+7, v3.end());
    p_v(v3);

    // merge
    v3.empty();
    std::sort(v.begin(), v.end());
    std::sort(v2.begin(), v2.end());
    std::merge(v.begin(), v.end(), v2.begin(), v2.end(), v3.begin());
    p_v(v3);

    // remove. 查找得到第一个元素的位置，然后从此位置开始遍历容器，将后面的元素前移。
    // 跳过和value相同的值。最后remove返回“指向最后一个有用元素的iterator”.
    // 在remove算法中没有修改原容器的size及end(). 实际上返回的位置到end()之间的元素已经没有意义。
    std::vector<int> v4 = v3;
    // erase做真正的删除
    v4.erase(std::remove(v4.begin(), v4.end(), 20), v4.end());
    p_v(v4);

    return 0;
}
