Reference Coreference Scorer
============================

DESCRIPTION
-----------

This is a modified version of the official implementation of the
revised coreference scorer used for CoNLL-2011/2012 shared tasks on
coreference resolution. It handles discontinuous mentions, i.e. mentions
composed of non-contiguous tokens, and has implemented an optional
partial mention matching scheme.


VERSION
-------

The current stable (official) version for scoring predicted mentions is **v8.02**

CITATION
--------

We would appreciate if you cite the paper when you use this scorer as
some of us are academics or wanting to be academics, and citations
matter.

  ::

   @InProceedings{pradhan-EtAl:2014:P14-2,
     author    = {Pradhan, Sameer  and  Luo, Xiaoqiang  and  Recasens, Marta  and  Hovy, Eduard  and  Ng, Vincent  and  Strube, Michael},
     title     = {Scoring Coreference Partitions of Predicted Mentions: A Reference Implementation},
     booktitle = {Proceedings of the 52nd Annual Meeting of the Association for Computational Linguistics (Volume 2: Short Papers)},
     month     = {June},
     year      = {2014},
     address   = {Baltimore, Maryland},
     publisher = {Association for Computational Linguistics},
     pages     = {30--35},
     url       = {http://www.aclweb.org/anthology/P14-2006}
     }


USAGE
-----

  ::

     perl scorer.pl <metric> <key> <response> [<document-id>]


     <metric>: the metric desired to score the results. one of the following values:

     muc: MUCScorer (Vilain et al, 1995)
     bcub: B-Cubed (Bagga and Baldwin, 1998)
     ceafm: CEAF (Luo et al., 2005) using mention-based similarity
     ceafe: CEAF (Luo et al., 2005) using entity-based similarity
     blanc: BLANC (Luo et al., 2014) BLANC metric for gold and predicted mentions
     all: uses all the metrics to score

     <key>: file with expected coreference chains in CoNLL-2011/2012 format

     <response>: file with output of coreference system (CoNLL-2011/2012 format)

     <allow_partial>: if 'true', partial mention matches are allowed; otherwise
                      response mentions must match key mention spans exactly
 
     <document-id>: optional. The name of the document to score. If name is not
                    given, all the documents in the dataset will be scored. If given
                    name is "none" then all the documents are scored but only total
                    results are shown.

INPUT
-----

The input expected complies with the format used by the CoNLL 2011 and 2012
shared tasks on coreference resolution as described here:
http://conll.cemantix.org/2012/data.html (See the "*_conll File Format" heading).
The coreference information is stored in the final column of the file using
parentheses and identifiers to indicate the begin and end tokens for mentions
in a coreference chain. This particular version of the scoring software has been
augmented to support discontinuous mentions, i.e. mentions composed of non-
contiguous tokens. Discontinuous mentions are indicated by the addition of a
sequence of one or more characters after the chain identifier, e.g. in the test
document shown below, the entity chain with ID 0 is composed of 5 mentions:
    1) a mention spanning tokens 0-1
    2) a discontinuous mention composed of token 3 and tokens 5-7
    3) another discontinuous mention composed of tokens 9 and 11
    4) a mention at token 14
    5) a mention spanning tokens 16-18

  ::

    # begin document;
    test1	0	0	a	(0
    test1	0	1	b	0)
    test1	0	2	c	-
    test1	0	3	d	(0a)
    test1	0	4	e	-
    test1	0	5	f	(0a
    test1	0	6	g	-
    test1	0	7	h	0a)
    test1	.	8	.	-

    test2	0	0	i	(0b)
    test2	0	1	j	-
    test2	0	2	k	(0b)
    test2	0	3	l	-
    test2	0	4	m	-
    test2	0	5	n	(0)
    test2	0	6	o	-
    test2	0	7	p	(0
    test2	0	8	q	-
    test2	0	9	r	0)
    test2	0	10	.	-
    #end document

OUTPUT
------

The score subroutine returns an array with four values in this order:

Coreference Score
~~~~~~~~~~~~~~~~~

  ::

    Recall = recall_numerator / recall_denominator
    Precision = precision_numerator / precision_denominator
    F1 = 2 * Recall * Precision / (Recall + Precision)

These values are to standard output when variable ``$VERBOSE`` is not null.


Identification of Mentions
~~~~~~~~~~~~~~~~~~~~~~~~~~

A score for identification of mentions (recall, precision and F1) is
also included.  Mentions from system response are compared with key
mentions. This version performs strict and partial mention matching
depending on the setting of the <allow_partial> input argument. Note
that strict mention matching was was used in the CoNLL-2011 and 2012
shared tasks.

AUTHORS
-------

* Emili Sapena, Universitat Politècnica de Catalunya, http://www.lsi.upc.edu/~esapena, esapena <at> lsi.upc.edu
* Sameer Pradhan, http://cemantix.org, pradhan <at> cemantix.org
* Sebastian Martschat, sebastian.martschat <at> h-its.org
* Xiaoqiang Luo, xql <at> google.com


COPYRIGHT
---------

  ::

    2009-2011, Emili Sapena esapena <at> lsi.upc.edu
    2011-      Sameer Pradhan pradhan <at> cemantix.org
