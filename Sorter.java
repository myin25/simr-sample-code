/**
* Sorter contains various implementations of sorting algorithms, including:
*       selectionSort(Comparable[] a)
*       insertionSort(Comparable[] a)
*       mergeSort(Comparable[] a)
*       quickSort(Comparable[] a)
* Other methods in this class include:
*       main(String[] args)
*       Sorter()
*       indexOfMin(Comparable[] a, int startIndex)
*       insert(Comparable[] a, int nextIndex)
*       mergesortHelp(Comparable[] a, int lowIndex, int highIndex)
*       merge(Comparable[] a, int lowIndex, int midIndex, int highIndex)
*       partition(Comparable[] a, int lowIndex, int highIndex)
*       quicksortHelp(Comparable[] a, int lowIndex, int highIndex)
* @author Anu Datar
* @author Melody Yin
* @version 2-24-22
*/
public class Sorter
{
    private SortDisplay display;
    
    /**
    * main method instantiates a sorter instance
    * Usage: called directly by the IDE or when Java is launched 
    * ------------------------------------------
    * Creates a Sorter object, but calls no methods from Sorter 
    * because the GUI SortDisplay calls sort methods in Sorter
    * 
    * @param args an array of arguments for legacy command line
    *              the values are not used
    */
    public static void main(String[] args)
    {
        Sorter sorter = new Sorter();
    }
    
    /**
    * Constructor: Sorter()
    * Usage:  Sorter aSorter = new Sorter()
    * ________________________________________
    * Constructor for Sorter objects.  Creates a new display, which controls
    * all of the sorting by means of call-backs to this class.
    */
    public Sorter()
    {
        display = new SortDisplay(this);
    }

    /**
     * Method: indexOfMin()
     * Usage: aSorter.indexOfMin(Comparable[] a, int startIndex);
     * ________________________________________
     * Takes in an array of Comparable objects and a starting index and finds
     *      the index of the smallest value in the array by iterating
     *      through each element in the array. The range of elements being
     *      considered starts at startIndex and goes until the end of the 
     *      array.
     * @param a                 the array that we are looking for the min
     *                              value in
     * @param startIndex        the starting index for iteration
     * @precondition 0 < startIndex < a.length
     * @return the index of the smallest value; if the length of the array is
     *      zero, return -1.
     */
    public int indexOfMin(Comparable[] a, int startIndex)
    {
        // if the array has a length of 0, then there is no min value
        //      return -1 (element not found)
        if (a.length == 0 || startIndex < 0 || startIndex >= a.length)
        {
            System.out.println("out of bounds error");
            return -1;
        }
        
        // linear search for the minimum value
        int minIndex = startIndex;
        for (int i = startIndex; i < a.length; i++)
        {
            System.out.println("minindex is " + minIndex);
            if (a[minIndex].compareTo(a[i]) > 0)
            {
                minIndex = i;
            }
        }
        return minIndex;
    }

    /**
     * Method: selectionSort(Comparable[] a);
     * Usage: aSorter.selectionSort(Comparable[] a);
     * ________________________________________
     * Takes in an array of Comparable objects and sorts them using selection
     *      sort, which iterates through each possible start index, beginning
     *      with 0 and ending at a.length - 1 and finds the minimum for that 
     *      subarray. It then swaps that minimum with the start index. This 
     *      process repeats until the start index is a.length - 1.
     * @param a                 the array that we are sorting
     * @postcondition the given array is sorted
     */
    public void selectionSort(Comparable[] a)
    {
        int minIndex;
        Comparable temp;
        
        // iterate through each possible start index and find the min
        for (int startIndex = 0; startIndex < a.length; startIndex++)
        {
            minIndex = indexOfMin(a, startIndex);
            
            // swap the min with the startIndex
            temp = a[startIndex];
            a[startIndex] = a[minIndex];
            a[minIndex] = temp;
            display.update();
        }
    }

    /**
     * Method: insert()
     * Usage: aSorter.insert(Comparable[] a, int nextIndex);
     *      Typically used in insertionSort() in order to 'slide'an element 
     *      into place.
     * ________________________________________
     * Takes in an array of Comparable objects. Given an object at index
     *      nextIndex, insert iterates through each item in the array before
     *      nextIndex + 1 and 'slides' the object until it finds a place such
     *      that the section of the array from 0 to nextIndex (inclusive) is 
     *      in strictly increasing order.
     * @param a                 the array that we are inserting the element
     *                              into
     * @param nextIndex         the element of the item we are 'sliding' into
     *                              place
     * @precondition All of the elements in the array before nextIndex will 
     *      already appear in increasing order and the remaining elements 
     *      will appear in random order. nextIndex is within the index bounds
     *      of the array. a has been initalized.
     * @postcondition The element at nextIndex will be inserted into its 
     *      proper place in subarray in such a way that that the section of 
     *      the array up to nextIndex (inclusive) will be sorted in ascending
     *      order.
     */
    public void insert(Comparable[] a, int nextIndex)
    {
        Comparable toinsert = a[nextIndex];
        System.out.println("toinsert " + toinsert);
        int temp = nextIndex - 1;
        while (temp >= 0 && (a[temp]).compareTo(toinsert) > 0)
        {
            System.out.println("moving over");
            a[temp + 1] = a[temp];
            temp -= 1;
        }
        a[temp + 1] = toinsert;
    }

    /**
     * Method: insertionSort()
     * Usage: aSorter.insertionSort(Comparable[] a);
     * ________________________________________
     * Takes in an array of Comparable objects and sorts them using insertion
     *      sort. It inserts each element of the array into a growing 
     *      sequence of sorted values. Insertion sort works by assuming that
     *      the front part of the list is always sorted, taking the first 
     *      item from the part of the list that is assumed to be unsorted, 
     *      and inserting it into its proper place by 'sliding' each item 
     *      larger than it one position to the right. When the correct spot 
     *      for the item is determined (when i is an index such that 
     *      array[i - 1] is either out of bounds or less than/equal to 
     *      array[i] and array[i + 1] is either out of bounds or larger than
     *      array[i]), the item is added there. The insertion of the item in 
     *      its correct place in the sorted portion of the array is 
     *      implemented using the insert() method.
     * @param a                 the array that we are sorting
     * @postcondition The given array is sorted in ascending order.
     */
    public void insertionSort(Comparable[] a)
    {
        Comparable curr;
        
        for (int i = 1; i < a.length; i++)
        {
            // insert each element into the correct place
            insert(a, i);
            display.update();
        }
    }

    /**
     * Method: mergesort()
     * Usage: aSorter.mergesort(Comparable[] a);
     * ________________________________________
     * Sorts an array using mergesort. Mergesort works like this: If the 
     *      length of the given array is 1 or less, return the array. If the 
     *      length of the given array is larger than 1, then split the array
     *      in two equal halves (or as equal as possible if the number of 
     *      elements is odd) and call mergesort on those halves to sort them.
     *      After this, merge the now sorted halves by repeatedly comparing
     *      the first items of each half and moving the smaller of the two
     *      to the front of the array. Once one of the halves runs out of
     *      items to compare, add all the remaining items in the other half
     *      to the array. You keep repeating this process recursively until
     *      the original array given by the user is sorted. mergesort()
     *      itself simply calls on a helper method, which is the actual
     *      implementation of the algorithm. The reason for this is that 
     *      mergesort() only has one parameter, the array to sort, in order
     *      to make the user's interactions with the function more simple.
     * @param a                 the array that we are sorting
     * @postcondition the array a is now sorted in ascending order.
     */
    public void mergesort(Comparable[] a)
    {
        mergesortHelp(a, 0, a.length - 1);
    }

    /**
     * Method: mergesortHelp()
     * Usage: mergesortHelp(a, int lowIndex, int highIndex);
     * 
     */
    private void mergesortHelp(Comparable[] a, int lowIndex, int highIndex)
    {   
        if (lowIndex < highIndex) {
            int mid = lowIndex + (highIndex - lowIndex) / 2;
  
            mergesortHelp(a, lowIndex, mid);
            mergesortHelp(a, mid + 1, highIndex);

            merge(a, lowIndex, mid, highIndex);
        }
    }
    
    /**
    * method merge()
    * Useage: merge(inputArray, lowIndex, midIndex, highIndex)
    *_______________________________________________
    * Merges the two halves of the input array into one.  The method assumes
    * that each half of the input array is sorted as follows:
    * 
    *                a[lowIndex] to a[midIndex] are in increasing order.
    *                a[midIndex + 1] to a[highIndex] are in increasing order.
    * The method creates an array to hold the output.  It then establishes 
    * two pointers into the two halves of the input array.  The values at the
    * pointer locations are compared, and the smallest is added to the output
    * array.  The corresponding pointer is then increased by one.  In the event
    * either half becomes empty, the remaining values are copied to the output
    * array.
    * Postcondition: a[lowIndex] to a[highIndex] are in increasing order.
    *
    * @param a is the input array of Comparable values
    * @param lowIndex is the index into the array a corresponding to the 
    *       beginning of the first half of the array to merge
    * @param midIndex is the index of the last value in the first half of the
    *       array
    * @param highIndex is the index of the last value in the second half of 
    *       the array
    */
    private void merge(Comparable[] a, int lowIndex, int midIndex, 
                            int highIndex)
    {
        Comparable[] copy = new Comparable[a.length];
        for (int i = lowIndex; i <= highIndex; i++)
            copy[i] = a[i];
        int left = lowIndex;
        int right = midIndex + 1;
        for (int i = lowIndex; i <= highIndex; i++)
        {
            if (right > highIndex ||
                (left <= midIndex && copy[left].compareTo(copy[right]) < 0))
            {
                a[i] = copy[left];
                left++;
            }
            else
            {
                a[i] = copy[right];
                right++;
            }
            display.update();
        }
    }

    /**     
     * Method: quicksort()
     * Usage: sorter.quicksort(inputArray)
     * -------------------------------------
     * quicksort() does not actual do the sorting,
     * just calls quicksortHelp with parameters (a, 0, a.length-1),
     * which does the actual quick sorting
     * 
     * Postcondition: a[lowIndex] to a[highIndex] are in increasing order
     * @param a - array of comparable elements to be sorted with quick sort
     */
    public void quicksort(Comparable[] a)
    {
        /* To be implemented post the AP Exam */
    }

    /**
     * Method: quicksortHelp()
     * Usage: quicksortHelp(a, low, high)
     * ------------------------------------------
     * Quick sorting is a recursive sorting algorithm that sets a pivot point
     *      (lowIndex in this case) and calls partition which performs rough 
     *      sort: puts every element less than pivot left of pivot, and every 
     *      element bigger than pivot right of pivot. Then, quicksortHelp is 
     *      called on the sections left & right of the pivot point
     * Base case: section of the array given by low & highIndex has 1 element
     *         (high <= low), which is "sorted" by definition. Therefore, 
     *         nothing is done to it.
     * Recursive reduction: the element at lowIndex is sorted as the pivot 
     *         using partition() and the index where it lands is returned.
     *         The array is then divided from (low,pivot-1) & (pivot+1,high)
     *         because index pivot is already sorted, and quicksortHelp is 
     *         used again on sections left & right of the pivot element.
     * 
     * Postcondition: a[lowIndex] to a[highIndex] are in increasing order
     * @param a - array of comparable elements to be sorted with quick sort
     * @param lowIndex - beginning index of section of array to be sorted
     * @param highIndex - ending index of section of array to be sorted
     */
    private void quicksortHelp(Comparable[] a, int lowIndex, int highIndex)
    {   
        /* To be implemented post the AP Exam */
    }
    
    /**
    * Method partition
    * Usuage: int pivotIndex = partition(a, lowIndex, highIndex)
    *___________________________________________________________
    *
    *Returns the index of the pivot element defined as follows:
    *                All elements on the left side of the pivot (from lowIndex)
    *                are less than or equal to the pivot.
    *                All elements on the right side of the pivot 
    *                   (through highIndex) are greater than or equal to the
    *                   pivot.
    * The computation is performed in place.
    * @param a the array to partion
    * @param lowIndex is the index of the start of the part of array a to
    *       consider
    * @param highIndex is the index of the end of the part of array a to 
    *       consider
    * @return the index of the pivot element in array a
    */
    private int partition(Comparable[] a, int lowIndex, int highIndex)
    {
        /* To be implemented post the AP Exam */
        return -1;
    }
}
