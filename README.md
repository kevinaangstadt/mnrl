# MNRL: MNRL Network Representation Language

MNRL is an open source, JSON-based representation for graph-based structures. It
is primarily used for representing finite automata; however, it is general
enough to support other use cases.  This repository contains the JSON
specification for MNRL and APIs for:

* C++ 
* Haskell
* Python2
* Python3

## Versions

- [v1.1.1](../../tree/v1.1.1): Updated C++ API that is significantly faster for loading
  and validating MNRL files.  **NOTE:** The API has changed slightly from v1.0
  and is **not** backwards-compatible.  We now use RapidJSON instead of JSON11
  and ValiJSON.
- [v1.0](../../tree/v1.0): Initial release

## License
This code is released under a BSD 3-Clause license. Please see [`LICENSE`](./LICENSE) for details. 

## Publications
The following publications are associated with MNRL:

- Kevin Angstadt, Jack Wadden, Vinh Dang, Tex Xie, Dan Kramp, Westley Weimer,
  Mircea Stan, and Kevin Skadron, "MNCaRT: An Open-Source, Multi-Architecture
  Automata-Processing Research and Execution Ecosystem," in IEEE Computer 
  Architecture Letters, vol. 17, no. 1, pp. 84-87, 2018.
  
  Bibtex:
    ```
    @ARTICLE{8166734, 
        author={K. Angstadt and J. Wadden and V. Dang and T. Xie and D. Kramp and W. Weimer and M. Stan and K. Skadron}, 
        journal={IEEE Computer Architecture Letters}, 
        title={{MNCaRT}: An Open-Source, Multi-Architecture Automata-Processing Research and Execution Ecosystem}, 
        year={2018}, 
        volume={17}, 
        number={1}, 
        pages={84-87}, 
        doi={10.1109/LCA.2017.2780105}, 
        ISSN={1556-6056}, 
        month={Jan}
    }
    ```

- Kevin Angstadt, Jack Wadden, Westley Weimer, and Kevin Skadron, "MNRL and
  MNCaRT: An open-source, multi-architecture state machine research and
  execution ecosystem," University of Virginia, Tech. Rep. CS2017-01, 2017.
  
  Bibtex:
    ```
    @techreport{mnrl,
        Author = {Angstadt, Kevin and Wadden, Jack and Weimer, Westley and Skadron, Kevin},
        Title = {{MNRL} and {MNCaRT}: An Open-Source, Multi-Architecture State Machine Research and Execution Ecosystem},
        Institution = {University of Virginia},
        Number = {CS2017-01},
        Year = {2017}
    }
    ```