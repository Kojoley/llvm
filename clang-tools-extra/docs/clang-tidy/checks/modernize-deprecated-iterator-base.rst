.. title:: clang-tidy - modernize-deprecated-iterator-base

modernize-deprecated-iterator-base
==================================

Finds deprecated in C++17 inheritance from ``std::iterator`` and replaces it
with type aliases.

Example
-------

.. code-block:: c++

  struct my_iterator : std::iterator<std::random_access_iterator_tag, int> {
    ...
  };

transforms to:

.. code-block:: c++

  struct my_iterator {
    using iterator_category = std::random_access_iterator_tag;
    using value_type        = int;
    using difference_type   = std::ptrdiff_t;
    using pointer           = int *;
    using reference         = int &;

    ...
  };

Known Limitations
-----------------

* Base class symbol ambiguities resolved with ``std::iterator`` values.

* Will not remove ``<iterator>`` include even if it is no longer needed.
