---
date: 2019-07-07
tags: programming java algorithm
---

# Sorts of Sorts

I wrote some of the popular sorting algorithms in Java for fun and practice.

```java
package net.abhinavsarkar.sorts;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.concurrent.ThreadLocalRandom;

public class Sorts {
  @FunctionalInterface
  interface Comparator<T> {
    int compare(T o1, T o2);
  }

  public static <T> T[] selectionSort(T[] input, Comparator<T> comparator) {
    for (int i = 0; i < input.length - 1; i++) {
      int j = findMinimumIndex(input, i, comparator);
      swap(input, i, j);
    }
    return input;
  }

  private static <T> int findMinimumIndex(
      T[] input, int idx, Comparator<T> comparator) {
    int minIdx = -1;
    T min = null;
    for (int j = idx; j < input.length; j++) {
      if (min == null || comparator.compare(input[j], min) < 0) {
        min = input[j];
        minIdx = j;
      }
    }
    return minIdx;
  }

  public static <T> T[] insertionSort(T[] input, Comparator<T> comparator) {
    for (int i = 1; i < input.length; i++) {
      for (int j = i; j > 0; j--) {
        if (comparator.compare(input[j], input[j - 1]) < 0) {
          swap(input, j, j - 1);
        } else {
          break;
        }
      }
    }
    return input;
  }

  public static <T> T[] mergeSort(T[] input, Comparator<T> comparator) {
    if (input.length == 0) {
      return input;
    }
    return mergeSort(input, 0, input.length, comparator);
  }

  private static <T> T[] mergeSort(
      T[] input, int start, int end, Comparator<T> comparator) {
    if (start == end - 1) {
      T[] output = mkArray(input, 1);
      output[0] = input[start];
      return output;
    }

    int half = start + (end - start) / 2;
    T[] left = mergeSort(input, start, half, comparator);
    T[] right = mergeSort(input, half, end, comparator);
    return merge(left, right, comparator);
  }

  private static <T> T[] merge(T[] left, T[] right, Comparator<T> comparator) {
    T[] output = mkArray(left, left.length + right.length);
    int i, j, k;
    i = j = k = 0;
    while (i < left.length || j < right.length) {
      if (i >= left.length) {
        System.arraycopy(right, j, output, j + left.length, right.length - j);
        break;
      }
      if (j >= right.length) {
        System.arraycopy(left, i, output, i + right.length, left.length - i);
        break;
      }
      output[k++] = comparator.compare(left[i], right[j]) <= 0 
        ? left[i++] : right[j++];
    }

    return output;
  }

  @SuppressWarnings("unchecked")
  private static <T> T[] mkArray(T[] input, int length) {
    return (T[]) Array.newInstance(input.getClass().getComponentType(), length);
  }

  public static <T> T[] quickSort(T[] input, Comparator<T> comparator) {
    if (input.length <= 1) {
      return input;
    }

    shuffle(input);
    return quickSort(input, 0, input.length, comparator);
  }

  private static <T> void shuffle(T[] input) {
    for (int i = input.length - 1; i > 0; i--) {
      int j = ThreadLocalRandom.current().nextInt(i + 1);
      swap(input, i, j);
    }
  }

  private static <T> T[] quickSort(
      T[] input, int start, int end, Comparator<T> comparator) {
    if (end - start <= 1) {
      return input;
    }

    int pivot = partition(input, start, end, comparator);
    quickSort(input, start, pivot, comparator);
    quickSort(input, pivot, end, comparator);
    return input;
  }

  private static <T> int partition(
      T[] input, int start, int end, Comparator<T> comparator) {
    int pivot = end - 1;
    int firstHigh = start;
    for (int i = start; i < pivot; i++) {
      if (comparator.compare(input[i], input[pivot]) < 0) {
        swap(input, i, firstHigh);
        firstHigh++;
      }
    }
    swap(input, pivot, firstHigh);

    return firstHigh;
  }

  private static <T> void swap(T[] input, int i, int j) {
    if (i == j) {
      return;
    }

    T temp = input[i];
    input[i] = input[j];
    input[j] = temp;
  }

  public static void main(String[] args) {
    AtomicInteger comparisons = new AtomicInteger(0);
    Comparator<String> comparator = (s1, s2) -> {
      comparisons.getAndIncrement();
      return s2.compareTo(s1);
    };

    comparisons.set(0);
    System.out.printf("selectionSort\n Result = %s\n Comparisons = %d\n",
      Arrays.toString(selectionSort(mkInput(), comparator)), comparisons.get());

    comparisons.set(0);
    System.out.printf("insertionSort\n Result = %s\n Comparisons = %d\n",
      Arrays.toString(insertionSort(mkInput(), comparator)), comparisons.get());

    comparisons.set(0);
    System.out.printf("mergeSort\n Result = %s\n Comparisons = %d\n",
      Arrays.toString(mergeSort(mkInput(), comparator)), comparisons.get());

    comparisons.set(0);
    System.out.printf("quickSort\n Result = %s\n Comparisons = %d\n",
      Arrays.toString(quickSort(mkInput(), comparator)), comparisons.get());

  }

  private static String[] mkInput() {
    return new String[]{"abhinav", "sarkar", "barista", "jordan", "data",
                        "cata", "meta", "dota", "best", "recursion"};
  }
}
```

Output:
```plain
selectionSort
 Result = [sarkar, recursion, meta, jordan, dota, data, cata, best, barista, abhinav]
 Comparisons = 45
insertionSort
 Result = [sarkar, recursion, meta, jordan, dota, data, cata, best, barista, abhinav]
 Comparisons = 35
mergeSort
 Result = [sarkar, recursion, meta, jordan, dota, data, cata, best, barista, abhinav]
 Comparisons = 24
quickSort
 Result = [sarkar, recursion, meta, jordan, dota, data, cata, best, barista, abhinav]
 Comparisons = 26
```
