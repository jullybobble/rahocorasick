package ahocorasick;

import com.hankcs.algorithm.AhoCorasickDoubleArrayTrie;

import java.util.HashMap;

class AhoCorasickWrapper {
    public static References detect(String[] texts, AhoCorasickDoubleArrayTrie<String> trie) {
        References refs = new References(texts.length);
        for(int i = 0; i < texts.length; i++) {
            refs.addReferences(i, trie.parseText(texts[i]));
        }
        return refs;
    }

    public static AhoCorasickDoubleArrayTrie<String> build(String[] keys, String[] values) {
        if(keys.length == values.length) {
            HashMap<String, String> map = new HashMap<String, String>(keys.length);
            for(int i = 0; i < keys.length; i++) {
                map.put(keys[i], values[i]);
            }
            AhoCorasickDoubleArrayTrie<String> trie = new AhoCorasickDoubleArrayTrie<String>();
            trie.build(map);
            return trie;
        } else {
            throw new IllegalArgumentException("keys and values don't have the same length");
        }
    }

    public static AhoCorasickDoubleArrayTrie<String> build(String[] keys) {
        return build(keys, new String[keys.length]);
    }
}
