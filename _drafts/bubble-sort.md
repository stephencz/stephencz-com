---
layout: page
title: Bubble Sort
group: "sort-algorithms"
---

Bubble sort is a simple, but inefficient, sorting algorithm.


## How Does It Work?

The algorithm works by repeatedly iterating over an array and comparing the current value with the next.
If the current value is larger than the next value, their positions are swapped. 

For example, consider an array containing these five integers: **[4, 3, 5, 1, 2]**.
One pass over this array would look like this:

```markdown
[4, 3, 5, 1, 2] // Is 4 larger than 3? Yes, swap.
[3, 4, 5, 1, 2] // Is 4 larger than 5? No, don't swap.
[3, 4, 5, 1, 2] // Is 5 larger than 1? Yes, swap.
[3, 4, 1, 5, 2] // Is 5 larger than 2? Yes, swap.
[3, 4, 1, 2, 5] // Pass complete
```

Given an array of size *n*, an unoptimized bubble sort algorithm performs *(n - 1)* passes.
Let's see another pass on our array:

```
[3, 4, 1, 2, 5] // Is 3 larger than 4? No, don't swap.
[3, 4, 1, 2, 5] // Is 4 larger than 1? Yes, swap.
[3, 1, 4, 2, 5] // Is 4 larger than 2? Yes, swap.
[3, 1, 2, 4, 5] // Is 4 larger than 5? No, don't swap.
[3, 1, 2, 4, 5] // Pass complete.
```

The bubble sort algorithm guarantees that at least one value will be sorted each pass.
Specifically, the next highest unsorted value, will be sorted.
This results in largest integers "bubbling" to the top of the array.

<figure markdown="0">
<img src="https://cdn.stephencz.com/sort-algorithms/bubble-sort-1.gif" width="100%">
<figcaption>This figure shows the state of the array after each pass. The next largest unsorted integer always "bubbles" to the top of the array.</figcaption>
</figure>

<figure markdown="0">
<img src="https://cdn.stephencz.com/sort-algorithms/bubble-sort-2.gif" width="100%">
<figcaption>This figure shows the individual swaps that take place during each pass until the entire array is sorted.</figcaption>
</figure>

Because our example array has a size of five, it will take at most three more passes to completely sort the array.

```
[3, 1, 2, 4, 5] // Is 3 larger than 1? Yes, swap.
[1, 3, 2, 4, 5] // Is 3 larger than 2? Yes, swap.
[1, 2, 3, 4, 5] // Our array is now sorted
```

As it turns out, we only needed one more pass to fully sort our data.
An unoptimized bubble sort would have continued comparing integers and performs the last two passes.
However, the bubble sort algorithm can be optimized to stop sorting if no swaps are made during a pass.
And it can be further optimzied to not waste time comparing already sorted integers.

## Basic Implementation

A basic bubble sort implement consists of four key parts:

1. The outer loop which iterates *(n - 1)* times.
2. The inner loop which also iterates *(n - 1)* times.
3. An if statement or comparison to check if the current value is larger than the next value.
4. Code to swap the two values.

Because 

```c++
void bubbleSort(vector<int> &data)
{
    int n = data.size();
    for(int i = 0; i < (n - 1); i++)
    {
        for(int j = 0; j < (n - 1); j++)
        {
            if(data[j] > data[j + 1])
            {
                int temp = data[j];
                data[j] = data[j + 1];
                data[j + 1] = temp;
            }
        }
    }
}
```

## Optimized Implementation

```c++
void optimizedBubbleSort(vector<int> &data)
{
    int n = data.size();
    for(int i = 0; i < (n - 1); i++)
    {
        bool swapped = false;
        for(int j = 0; j < (n - 1); j++)
        {
            if(data[j] > data[j + 1])
            {
                int temp = data[j];
                data[j] = data[j + 1];
                data[j + 1] = temp;

                swapped = true;
            }
        }

        if(!swapped)
        {
            break;
        }
    }
}
```

```c++
void optimizedBubbleSort(vector<int> &data)
{
    int n = data.size();
    int sorted = n;

    for(int i = 0; i < (n - 1); i++)
    {
        bool swapped = false;
        for(int j = 0; j < (sorted - 1); j++)
        {
            if(data[j] > data[j + 1])
            {
                int temp = data[j];
                data[j] = data[j + 1];
                data[j + 1] = temp;

                swapped = true;
            }
        }

        if(!swapped)
        {
            break;
        }

        sorted++;
    }
}
```

## Time Complexity

* **Worst Case:** *O(n^2)*
* **Best Case:** *O(n)* 
