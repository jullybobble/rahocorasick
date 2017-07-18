package ahocorasick;

import com.hankcs.algorithm.AhoCorasickDoubleArrayTrie;

import java.util.ArrayList;
import java.util.List;

public class References {
    private List<Integer> textIndex;
    private List<String> value;
    private List<Integer> begin;
    private List<Integer> end;

    public References(int initialCapacity) {
        this.textIndex = new ArrayList<Integer>(initialCapacity);
        this.value = new ArrayList<String>(initialCapacity);
        this.begin = new ArrayList<Integer>(initialCapacity);
        this.end = new ArrayList<Integer>(initialCapacity);
    }

    public String[] getValue() {
        return value.toArray(new String[0]);
    }

    public int[] getBegin() {
        return toArray(begin);
    }

    public int[] getEnd() {
        return toArray(end);
    }

    public int[] getTextIndex() {
        return toArray(textIndex);
    }

    private int[] toArray(List<Integer> list) {
        int[] array = new int[list.size()];
        int i = 0;
        for(int el: list) {
            array[i++] = el;
        }
        return array;
    }

    protected void addReferences(int textIndex, List<AhoCorasickDoubleArrayTrie<String>.Hit<String>> hits) {
        for(AhoCorasickDoubleArrayTrie<String>.Hit<String> hit: hits) {
            this.textIndex.add(textIndex);
            this.value.add(hit.value);
            this.begin.add(hit.begin);
            this.end.add(hit.end);
        }
    }

}

