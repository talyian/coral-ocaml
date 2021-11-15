

#include <cstdint>
#include <algorithm>
#include <unordered_map>

template <typename Key, typename Value, std::size_t capacity>
struct DictionaryData {

  Key keys[capacity];
  Value values[capacity];
  bool filled[capacity];

  std::int32_t mod_capacity(std::int32_t index) { return index % capacity; }
  struct Item { Key k; Value v; };
  void find(Key k) {
    int hash = std::hash<Key>(k);
    for(int i = hash; i < capacity; i ++) {
      if (!filled[i]) return;
      if (keys[i] == k) return;
    }
    for(int i = 0; i < hash; i ++) {

    }
  }
};
