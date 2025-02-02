---
date: 2019-07-07
tags: programming java algorithms
---

# Sorts of Sorts

I wrote some of the popular sorting algorithms in Java for fun and practice:
- Selection sort
- Insertion sort
- Merge sort
- Quick sort
- Heap sort

```java
package net.abhinavsarkar.sorts;

import java.io.IOException;
import java.lang.reflect.Array;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiFunction;
import java.util.stream.Stream;

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
  
  public static <T> T[] heapSort(T[] input, Comparator<T> comparator) {
    if (input.length <= 1) {
      return input;
    }

    heapify(input, comparator);
    deheapify(input, comparator);
    return input;
  }

  private static <T> void heapify(T[] input, Comparator<T> comparator) {
    for (int i = (input.length - 1)/2; i >= 0; i--) {
      bubbleDown(input, i, input.length, comparator);
    }
  }

  private static <T> void deheapify(T[] input, Comparator<T> comparator) {
    for (int i = input.length; i > 1; i--) {
      swap(input, 0, i - 1);
      bubbleDown(input, 0, i - 1, comparator);
    }
  }

  private static <T> void bubbleDown(
      T[] input, int index, int size, Comparator<T> comparator) {
    int maxIdx = index;
    maxIdx = getMaxIdx(input, index * 2 + 1, maxIdx, size, comparator);
    maxIdx = getMaxIdx(input, index * 2 + 2, maxIdx, size, comparator);
    if (index != maxIdx) {
      swap(input, index, maxIdx);
      bubbleDown(input, maxIdx, size, comparator);
    }
  }

  private static <T> int getMaxIdx(
      T[] input, int childIndex, int minIndex, int size, 
      Comparator<T> comparator) {
    return childIndex < size 
              && comparator.compare(input[minIndex], input[childIndex]) < 0 ?
            childIndex : minIndex;
  }

  private static <T> void swap(T[] input, int i, int j) {
    if (i == j) {
      return;
    }

    T temp = input[i];
    input[i] = input[j];
    input[j] = temp;
  }

  // let's sort 10000 shuffled words with each algorithm
  public static void main(String[] args) throws IOException {
    AtomicInteger comparisons = new AtomicInteger(0);
    Comparator<String> comparator = (s1, s2) -> {
      comparisons.getAndIncrement();
      return s2.compareTo(s1);
    };

    String[] input, output;
    try (Stream<String> lines = Files.lines(Paths.get("/usr/share/dict/words"))) {
      input = lines.limit(10000).toArray(String[]::new);
    }
    shuffle(input);

    String[] expected = arraySort(copyInput(input));

    runSort("build-in sort",
      Sorts::arraySort, comparator, comparisons, input, expected);
    runSort("selectionSort",
      Sorts::selectionSort, comparator, comparisons, input, expected);
    runSort("insertionSort",
      Sorts::insertionSort, comparator, comparisons, input, expected);
    runSort("mergeSort",
      Sorts::mergeSort, comparator, comparisons, input, expected);
    runSort("quickSort",
      Sorts::quickSort, comparator, comparisons, input, expected);
    runSort("heapSort",
      Sorts::heapSort, comparator, comparisons, input, expected);
  }

  private static void runSort(String title,
      BiFunction<String[], Comparator<String>, String[]> sorter,
      Comparator<String> comparator,
      AtomicInteger comparisonCounter,
      String[] input,
      String[] expected) {
    comparisonCounter.set(0);
    String[] output = sorter.apply(copyInput(input), comparator);
    System.out.printf("%s\n Match = %s\n Comparisons = %d\n",
      title, Arrays.equals(output, expected), comparisonCounter.get());
  }

  private static String[] arraySort(
      String[] input, Comparator<String> comparator) {
    Arrays.sort(input, comparator::compare);
    return input;
  }

  private static String[] copyInput(String[] input) {
    return Arrays.copyOf(input, input.length);
  }
}
```

Output:
```plain
build-in sort
 Match = true
 Comparisons = 120402
selectionSort
 Match = true
 Comparisons = 49995000
insertionSort
 Match = true
 Comparisons = 25146028
mergeSort
 Match = true
 Comparisons = 120359
quickSort
 Match = true
 Comparisons = 173217
heapSort
 Match = true
 Comparisons = 235382
```
