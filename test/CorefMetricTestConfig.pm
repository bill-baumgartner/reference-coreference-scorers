################################################################################
# This is the test configuration file. Test cases are stored in an
# array, each element consisting of:
#   (1) id: a unique identifier for the test case.
#   (2) key_file: the key file to be tested in the CoNLL format.
#   (3) response_file: the response file to be tested in the CoNLL format.
#   (4) expected_metrics: is a hash label from a metric name (identical to those
#                         used in the scorer.{pl|bat}) to an array of expected
#                         metric values. All metrics have 3 expected numbers:
#                         (recall, precision, F-measure).
################################################################################

package CorefMetricTestConfig;
use strict;
use warnings;
use Exporter;

our @ISA = qw( Exporter );

# these are exported by default.
our @EXPORT = qw(TestCases);

#
# Values following metric names are [recall, precision, F1]
#
our @TestCases = (
{ id => "A1", 
  key_file => "DataFiles/TC-A.key",
  response_file => "DataFiles/TC-A-1.response",
  expected_metrics => { "muc" => [1, 1, 1], 
                        "bcub" => [6/6, 6/6, 1],
                        "ceafm" => [1, 1, 1],
                        "ceafe" => [1, 1, 1],
                        "blanc" => [1, 1, 1],
                        "lea"  => [1, 1, 1] }
},
{ id => "A2", 
  key_file => "DataFiles/TC-A.key", 
  response_file => "DataFiles/TC-A-2.response",
  expected_metrics => { "muc" => [1/3, 1/1, 0.5], 
                        "bcub" => [(7/3)/6, 3/3, 14/25],
                        "ceafm" => [0.5, 1, 0.66667],
                        "ceafe" => [0.6, 0.9, 0.72],
                        "blanc" => [0.21591, 1, 0.35385],
                        "lea"   => [(1+3*(1/3))/6, 1, 0.5]}
},
{ id => "A3", 
  key_file => "DataFiles/TC-A.key", 
  response_file => "DataFiles/TC-A-3.response",
  expected_metrics => { "muc" => [3/3, 3/5, 0.75], 
                        "bcub" => [6/6, (4+7/12)/9, 110/163],
                        "ceafm" => [1, 0.66667, 0.8],
                        "ceafe" => [0.88571, 0.66429, 0.75918],
                        "blanc" => [1, 0.42593, 0.59717],
                        "lea"   => [1,(1+3*(1/3)+4*(3/6))/9, 2* (1+3*(1/3)+4*(3/6))/9/(1+(1+3*(1/3)+4*(3/6))/9)]}
},
{ id => "A4", 
  key_file => "DataFiles/TC-A.key",
  response_file => "DataFiles/TC-A-4.response",
  expected_metrics => { "muc" => [1/3, 1/3, 1/3], 
                        "bcub" => [(3+1/3)/6, (1+4/3+1/2)/7, 2*(5/9)*(17/42)/((5/9)+(17/42))],
                        "ceafm" => [0.66667, 0.57143, 0.61538],
                        "ceafe" => [0.73333, 0.55, 0.62857],
                        "blanc" => [0.35227, 0.27206, 0.30357],
                        "lea"   => [(1+2+0)/6,(1+3*(1/3)+2*0+0)/7, 2*0.5*2/7/(0.5+2/7)] }
},
{ id => "A5", 
  key_file => "DataFiles/TC-A.key", 
  response_file => "DataFiles/TC-A-5.response",
  expected_metrics => { "muc" => [1/3, 1/4, 2/7], 
                        "bcub" => [(3+1/3)/6, 2.5/8, 2*(5/9)*(5/16)/((5/9)+(5/16))],
                        "ceafm" => [0.66667, 0.5, 0.57143],
                        "ceafe" => [0.68889, 0.51667, 0.59048],
                        "blanc" => [0.35227, 0.19048, 0.24716],
                        "lea"   => [(1+2+3*0)/6,(1+4*(1/6)+2*0+1*0)/8, 2*0.5 *(5/24)/(0.5+(5/24))] }
},
{ id => "A6", 
  key_file => "DataFiles/TC-A.key", 
  response_file => "DataFiles/TC-A-6.response",
  expected_metrics => { "muc" => [1/3, 1/4, 2/7],
                        "bcub" => [(10/3)/6, (1+4/3+1/2)/8, 2*(5/9)*(17/48)/((5/9)+(17/48))],
                        "ceafm" => [0.66667, 0.5, 0.57143],
                        "ceafe" => [0.73333, 0.55, 0.62857],
                        "blanc" => [0.35227, 0.20870, 0.25817],
                        "lea"   => [(1+2+3*0)/6,(1+3/3+2*0+2*0)/8, 2*0.5*1/4/(0.5+1/4)] }
},
{ id => "A7", 
  key_file => "DataFiles/TC-A.key", 
  response_file => "DataFiles/TC-A-7.response",
  expected_metrics => { "muc" => [1/3, 1/3, 1/3], 
                        "bcub" => [(10/3)/6, (1+4/3+1/2)/7, 2*(5/9)*(17/42)/((5/9)+(17/42))],
                        "ceafm" => [0.66667, 0.57143, 0.61538],
                        "ceafe" => [0.73333, 0.55, 0.62857],
                        "blanc" => [0.35227, 0.27206, 0.30357],
                        "lea"   => [(1+2+3*0)/6, (1+3/3+2*0+1*0)/7, 2*0.5*2/7/(0.5+2/7)] }
},
{ id => "A8",
  key_file => "DataFiles/TC-A.key",
  response_file => "DataFiles/TC-A-8.response",
  expected_metrics => { "muc" => [1/3, 1/3, 1/3],
                        "bcub" => [(10/3)/6, (1+4/3+1/2)/7, 2*(5/9)*(17/42)/((5/9)+(17/42))],
                        "ceafm" => [0.66667, 0.57143, 0.61538],
                        "ceafe" => [0.73333, 0.55, 0.62857],
                        "blanc" => [0.35227, 0.27206, 0.30357],
                        "lea"   => [(1+2+3*0)/6, (1+3*(1/3)+2*0+1*0)/7, 2*0.5*2/7/(0.5+2/7)] }
},
{ id => "A9",
  key_file => "DataFiles/TC-A.key",
  response_file => "DataFiles/TC-A-9.response",
  expected_metrics => { "muc" => [1/3, 1/3, 1/3],
                        "bcub" => [(10/3)/6, (1+4/3+1/2)/7, 2*(5/9)*(17/42)/((5/9)+(17/42))],
                        "ceafm" => [0.66667, 0.57143, 0.61538],
                        "ceafe" => [0.73333, 0.55, 0.62857],
                        "blanc" => [0.35227, 0.27206, 0.30357],
                        "lea"   => [(1+2)/6, (1+3/3+0+0)/7, 2*0.5*2/7/(0.5+2/7)]}
},
{ id => "A10", 
  key_file => "DataFiles/TC-A.key", 
  response_file => "DataFiles/TC-A-10.response",
  expected_metrics => { "muc" => [0, 0, 0], 
                        "bcub" => [3/6, 6/6, 2/3],
                        #”ceafm" => [1, 1, 1],
                        #”ceafe" => [1, 1, 1],
                        "blanc" => [0.5, 0.36667, 0.42308], 
                        "lea"   => [1/6, 1/6, 2*1/6*1/6/(1/6+1/6)]}
},
{ id => "A11", 
  key_file => "DataFiles/TC-A.key", 
  response_file => "DataFiles/TC-A-11.response",
  expected_metrics => { "muc" => [3/3, 3/5, 6/8], 
                        "bcub" => [6/6, (1/6+2*2/6+3*3/6)/6, 14/25],
                        #”ceafm" => [1, 1, 1],
                        #”ceafe" => [1, 1, 1],
                        "blanc" => [0.5, 0.13333, 0.21053],
                        "lea"   => [(0+2+3)/6, 4/15, 2*5/6*4/15/(5/6+4/15)] }
},
{ id => "A12", 
  key_file => "DataFiles/TC-A.key", 
  response_file => "DataFiles/TC-A-12.response",
  expected_metrics => { "muc" => [0, 0, 0], 
                        "bcub" => [(1+1/2+2/3)/6, 4/7, 2*(13/36)*(4/7)/((13/36)+(4/7))],
                        #”ceafm" => [1, 1, 1],
                        #”ceafe" => [1, 1, 1],
                        "blanc" => [0.22727, 0.11905, 0.15625],
                        "lea"   => [1/6,1/7, 2*1/6*1/7/(1/6+1/7)] }
},
{ id => "A13", 
  key_file => "DataFiles/TC-A.key", 
  response_file => "DataFiles/TC-A-13.response",
  expected_metrics => { "muc" => [1/3, 1/6, 2/9], 
                        "bcub" => [(1+1/2+2*2/3)/6, (1/7+1/7+2*2/7)/7, 2*(17/36)*(6/49)/((17/36)+(6/49))],
                        #”ceafm" => [1, 1, 1],
                        #”ceafe" => [1, 1, 1],
                        "blanc" => [0.125, 0.02381, 0.04],
                        "lea"   => [(1*0+2*0+3/3)/6, 1/21, 2*1/6*1/21/(1/6+1/21)] }
},
{ id => "B1", 
  key_file => "DataFiles/TC-B.key", 
  response_file => "DataFiles/TC-B-1.response",
  expected_metrics => { #"muc" => [1, 1, 1], 
                        #"bcub" => [1, 1, 1],
                        #”ceafm" => [1, 1, 1],
                        #”ceafe" => [1, 1, 1],
                        "blanc" => [1/2 * (1/4 + 1/3), 1/2 * (1/4 + 1/3), 1/2 * (1/4 + 1/3)],
                        "lea"   => [(2*0+3/3)/5, (3*0+2)/5, 2*1/5*2/5/(1/5+2/5)]
 }
},
{ id => "C1", 
  key_file => "DataFiles/TC-C.key", 
  response_file => "DataFiles/TC-C-1.response",
  expected_metrics => { #"muc" => [1, 1, 1], 
                        #"bcub" => [1, 1, 1],
                        #”ceafm" => [1, 1, 1],
                        #”ceafe" => [1, 1, 1],
                        "blanc" => [1/2 * (2/5 + 10/16), 1/2 * (2/5 + 10/16), 1/2 * (2/5 + 10/16)],
                        "lea"   => [(2*0+3/3+2)/7, (3*0+2+2)/7, 2*3/7*4/7/(3/7+4/7)]
}
},
{ id => "D1", 
		key_file => "DataFiles/TC-D.key", 
		response_file => "DataFiles/TC-D-1.response",
 		expected_metrics => { "muc" => [9/9, 9/10, 2*(9/9)*(9/10)/(9/9+9/10)], 
                          "bcub" => [12/12, 16/21, 2*(12/12)*(16/21)/(12/12+16/21)],
                          #"ceafm" => [1, 1, 1],
                          #"ceafe" => [1, 1, 1],
                          #"blanc" => [1, 1, 1],
                          "lea"  => [(5+2+5)/12, (5+7*(11/21))/12, 2*1*(5+77/21)/12/(1+((5+77/21)/12))]
                        }
},
{ id => "E1", 
		key_file => "DataFiles/TC-E.key", 
		response_file => "DataFiles/TC-E-1.response",
 		expected_metrics => { "muc" => [9/9, 9/10, 2*(9/9)*(9/10)/(9/9+9/10)], 
                          "bcub" => [1, 7/12, 2*1*(7/12)/(1+7/12)],
                          #"ceafm" => [1, 1, 1],
                          #"ceafe" => [1, 1, 1],
                          #"blanc" => [1, 1, 1],
                          "lea" => [(5+2+5)/12, (10*(20/45)+2)/12, 2*1*((10*(20/45)+2)/12)/(1+((10*(20/45)+2)/12))]
                        }
},
{ id => "F1", 
		key_file => "DataFiles/TC-F.key", 
		response_file => "DataFiles/TC-F-1.response",
 		expected_metrics => { "muc" => [2/3, 2/2, 2*(2/3)*(2/2)/(2/3+2/2)] ,
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          #"blanc" => ,
                           "lea"   => [4*(2/6)/4, (2+2)/4, 2*2/6*1/(1+2/6)]
                        }
},
{ id => "G1", 
		key_file => "DataFiles/TC-G.key", 
		response_file => "DataFiles/TC-G-1.response",
 		expected_metrics => { "muc" => [2/2, 2/3, 2*(2/2)*(2/3)/(2/2+2/3)],
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          #"blanc" => ,
                           "lea"   => [1, (4*2/6)/4, 2*1*2/6/(1+2/6)]
                        }
},
{ id => "H1", 
		key_file => "DataFiles/TC-H.key", 
		response_file => "DataFiles/TC-H-1.response",
 		expected_metrics => { "muc" => [1, 1, 1],
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          #"blanc" => 
                          "lea"    => [1,1,1]
                        }
},
{ id => "I1", 
		key_file => "DataFiles/TC-I.key", 
		response_file => "DataFiles/TC-I-1.response",
 		expected_metrics => { "muc" => [2/3, 2/2, 2*(2/3)*(2/2)/(2/3+2/2)],
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          #"blanc" => ,
                           "lea"   => [4*(2/6)/4, (2+2)/4, 2*2/6*1/(2/6+1)]
                        }
},
{ id => "J1", 
		key_file => "DataFiles/TC-J.key", 
		response_file => "DataFiles/TC-J-1.response",
 		expected_metrics => { "muc" => [1/2, 1/1, 2*(1/2)*(1/1)/(1/2+1/1)],
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          #"blanc" => ,
                           "lea"   => [(3*1/3)/3, 1, 2*1/3/(1+1/3)]
                        }
},
{ id => "K1", 
		key_file => "DataFiles/TC-K.key", 
		response_file => "DataFiles/TC-K-1.response",
 		expected_metrics => { "muc" => [3/6, 3/6, 3/6],
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          #"blanc" => ,
                           "lea"   => [(7*(1+1+1)/21)/7, (3/3+3/3+3/3)/9, 2*3/21*3/9/(3/21+3/9)]
                        
                        }
},
{ id => "L1", 
		key_file => "DataFiles/TC-L.key", 
		response_file => "DataFiles/TC-L-1.response",
 		expected_metrics => { "muc" => [2/5, 2/4, 2*(2/5)*(2/4)/(2/5+2/4)],
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          #"blanc" => ,
                           "lea"   => [(3*1/3+4*1/6)/7, (2+2*0+3/3)/7, 2*(1+2/3)/7*3/7/(3/7+(1+2/3)/7)]
                        }
},
{ id => "M1", 
		key_file => "DataFiles/TC-M.key", 
		response_file => "DataFiles/TC-M-1.response",
 		expected_metrics => { "muc" => [1, 1, 1],
                          "bcub" => [1, 1, 1],
                          "ceafm" => [1, 1, 1],
                          "ceafe" => [1, 1, 1],
                          "blanc" => [1, 1, 1],
                          "lea"   => [1, 1, 1] 
}
},
{ id => "M2", 
		key_file => "DataFiles/TC-M.key", 
		response_file => "DataFiles/TC-M-2.response",
 		expected_metrics => { "muc" => [0, 0, 0],
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          "blanc" => [0, 0, 0],
                          "lea"   => [0, 0, 0] }
},
{ id => "M3", 
		key_file => "DataFiles/TC-M.key", 
		response_file => "DataFiles/TC-M-3.response",
 		expected_metrics => { #"muc" => ,
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          "blanc" => [0.26667, 1, 0.42105] ,
                          "lea"   => [6*(4/15)/6, (2+3+0)/6, 2*4/15*5/6/(4/15+5/6)]}
},
{ id => "M4", 
		key_file => "DataFiles/TC-M.key", 
		response_file => "DataFiles/TC-M-4.response",
 		expected_metrics => { #"muc" => ,
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          "blanc" => [0.2, 0.2, 0.2],
                          "lea"   => [6*(3/15)/6, 6*(3/15)/6, 2*3/15*3/15/(3/15+3/15)]}
},
{ id => "M5", 
		key_file => "DataFiles/TC-M.key", 
		response_file => "DataFiles/TC-M-5.response",
 		expected_metrics => { "muc" => [0, 0, 0],
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          "blanc" => [0, 0, 0],
                          "lea"   => [0, 0, 0] }
},
{ id => "M6", 
		key_file => "DataFiles/TC-M.key", 
		response_file => "DataFiles/TC-M-6.response",
 		expected_metrics => { #"muc" => ,
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          "blanc" => [0.06667, 0.25, 0.10526],
                          "lea"   => [6*(1/15)/6, (2+3*0+1*0)/6, 2*1/15*2/6/(1/15+2/6)] }
},
{ id => "N1", 
		key_file => "DataFiles/TC-N.key", 
		response_file => "DataFiles/TC-N-1.response",
 		expected_metrics => { "muc" => [0, 0, 0],
                          #"bcub" => [1, 1, 1],
                          #"ceafm" => [1, 1, 1],
                          #"ceafe" => [1, 1, 1],
                          "blanc" => [1, 1, 1],
                          "lea"   => [1, 1, 1]  }
},
{ id => "N2", 
		key_file => "DataFiles/TC-N.key", 
		response_file => "DataFiles/TC-N-2.response",
 		expected_metrics => { "muc" => [0, 0, 0],
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          "blanc" => [0, 0, 0],
                          "lea"   => [0, 0, 0]  }
},
{ id => "N3", 
		key_file => "DataFiles/TC-N.key", 
		response_file => "DataFiles/TC-N-3.response",
 		expected_metrics => { #"muc" => ,
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          "blanc" => [0.73333, 1, 0.84615],
                          "lea"   => [1/6, 1/6, 1/6] }
},
{ id => "N4", 
		key_file => "DataFiles/TC-N.key", 
		response_file => "DataFiles/TC-N-4.response",
 		expected_metrics => { "muc" => [0, 0, 0],
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          "blanc" => [0.2, 0.2, 0.2],
                          "lea"   => [3/6, 3/6, 3/6] }
},
{ id => "N5", 
		key_file => "DataFiles/TC-N.key", 
		response_file => "DataFiles/TC-N-5.response",
 		expected_metrics => { #"muc" => ,
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          "blanc" => [0, 0, 0],
                          "lea"   => [0, 0, 0] }
},
{ id => "N6", 
		key_file => "DataFiles/TC-N.key", 
		response_file => "DataFiles/TC-N-6.response",
 		expected_metrics => { #"muc" => ,
                          #"bcub" => ,
                          #"ceafm" => ,
                          #"ceafe" => ,
                          "blanc" => [0.13333, 0.18182, 0.15385],
                          "lea"   => [0, 0, 0] }
},
  {
    id            => "O1",
    key_file      => "DataFiles/TC-O.key",
    response_file => "DataFiles/TC-O-1.response",

   # key and response each contain one split-span mention (exact match expected)
    expected_metrics => {
      "muc"   => [1,     1,     1],
      "bcub"  => [6 / 6, 6 / 6, 1],
      "ceafm" => [1,     1,     1],
      "ceafe" => [1,     1,     1],
      "blanc" => [1,     1,     1]
    }
  },
  {
    id               => "O2",
    key_file         => "DataFiles/TC-O.key",
    response_file    => "DataFiles/TC-O-2.response",
    expected_metrics => {"muc" => [2 / 3, 2 / 3, 4 / 6]}

      #		"bcub" => [6/6, 6/6, 1],
      #		"ceafm" => [1, 1, 1],
      #		"ceafe" => [1, 1, 1],
      #		"blanc" => [1, 1, 1] }
  },
  {
    id               => "O2_partial",
    key_file         => "DataFiles/TC-O.key",
    response_file    => "DataFiles/TC-O-2.response",
    allow_partial    => "true",
    expected_metrics => {"muc" => [1, 1, 1]}

      #		"bcub" => [6/6, 6/6, 1],
      #		"ceafm" => [1, 1, 1],
      #		"ceafe" => [1, 1, 1],
      #		"blanc" => [1, 1, 1] }
  },
  {
    id               => "O3",
    key_file         => "DataFiles/TC-O.key",
    response_file    => "DataFiles/TC-O-3.response",
    expected_metrics => {"muc" => [2 / 3, 2 / 3, 4 / 6]}

      #		"bcub" => [6/6, 6/6, 1],
      #		"ceafm" => [1, 1, 1],
      #		"ceafe" => [1, 1, 1],
      #		"blanc" => [1, 1, 1] }
  },
  {
    id               => "O3_partial",
    key_file         => "DataFiles/TC-O.key",
    response_file    => "DataFiles/TC-O-3.response",
    allow_partial    => "true",
    expected_metrics => {"muc" => [2 / 3, 2 / 3, 4 / 6]}

      #		"bcub" => [6/6, 6/6, 1],
      #		"ceafm" => [1, 1, 1],
      #		"ceafe" => [1, 1, 1],
      #		"blanc" => [1, 1, 1] }
  },
  {
    id               => "P1",
    key_file         => "DataFiles/TC-P.key",
    response_file    => "DataFiles/TC-P-1.response",
    expected_metrics => {
      "muc"   => [1,     1,     1],
      "bcub"  => [6 / 6, 6 / 6, 1],
      "ceafm" => [1,     1,     1],
      "ceafe" => [1,     1,     1],
      "blanc" => [1,     1,     1]
    }
  },
  {
    id               => "P2",
    key_file         => "DataFiles/TC-P.key",
    response_file    => "DataFiles/TC-P-2.response",
    expected_metrics => {"muc" => [2 / 3, 2 / 3, 4 / 6]}

      #			"bcub" => [6/6, 6/6, 1],
      #			"ceafm" => [1, 1, 1],
      #			"ceafe" => [1, 1, 1],
      #			"blanc" => [1, 1, 1] }
  },
  {
    id               => "P2_partial",
    key_file         => "DataFiles/TC-P.key",
    response_file    => "DataFiles/TC-P-2.response",
    allow_partial    => "true",
    expected_metrics => {"muc" => [2 / 3, 2 / 3, 4 / 6]}

      #			"bcub" => [6/6, 6/6, 1],
      #			"ceafm" => [1, 1, 1],
      #			"ceafe" => [1, 1, 1],
      #			"blanc" => [1, 1, 1] }
  },
  {
    id               => "P3",
    key_file         => "DataFiles/TC-P.key",
    response_file    => "DataFiles/TC-P-3.response",
    expected_metrics => {"muc" => [1 / 3, 1 / 3, 2 / 6]}

      #			"bcub" => [6/6, 6/6, 1],
      #			"ceafm" => [1, 1, 1],
      #			"ceafe" => [1, 1, 1],
      #			"blanc" => [1, 1, 1] }
  },
  {
    id               => "P3_partial",
    key_file         => "DataFiles/TC-P.key",
    response_file    => "DataFiles/TC-P-3.response",
    allow_partial    => "true",
    expected_metrics => {"muc" => [1, 1, 1]}

      #			"bcub" => [6/6, 6/6, 1],
      #			"ceafm" => [1, 1, 1],
      #			"ceafe" => [1, 1, 1],
      #			"blanc" => [1, 1, 1] }
  },
  {
    id               => "P4",
    key_file         => "DataFiles/TC-P.key",
    response_file    => "DataFiles/TC-P-4.response",
    expected_metrics => {
      "muc"   => [1,     1,     1],
      "bcub"  => [6 / 6, 6 / 6, 1],
      "ceafm" => [1,     1,     1],
      "ceafe" => [1,     1,     1],
      "blanc" => [1,     1,     1]
    }
  },
  {
    id               => "P5",
    key_file         => "DataFiles/TC-P.key",
    response_file    => "DataFiles/TC-P-5.response",
    expected_metrics => {
      "muc"   => [1,     1,     1],
      "bcub"  => [6 / 6, 6 / 6, 1],
      "ceafm" => [1,     1,     1],
      "ceafe" => [1,     1,     1],
      "blanc" => [1,     1,     1]
    }
  },
  {
    id               => "Q1",
    key_file         => "DataFiles/TC-Q.key",
    response_file    => "DataFiles/TC-Q-1.response",
    expected_metrics => {
      "muc"   => [1,     1,     1],
      "bcub"  => [6 / 6, 6 / 6, 1],
      "ceafm" => [1,     1,     1],
      "ceafe" => [1,     1,     1],
      "blanc" => [1,     1,     1]
    }
  },
  {
    id               => "Q2",
    key_file         => "DataFiles/TC-Q.key",
    response_file    => "DataFiles/TC-Q-2.response",
    expected_metrics => {"muc" => [1 / 2, 1 / 2, 2 / 4]}

      #			"bcub" => [6/6, 6/6, 1],
      #			"ceafm" => [1, 1, 1],
      #			"ceafe" => [1, 1, 1],
      #			"blanc" => [1, 1, 1] }
  },
  {
    id               => "Q2_partial",
    key_file         => "DataFiles/TC-Q.key",
    response_file    => "DataFiles/TC-Q-2.response",
    allow_partial    => "true",
    expected_metrics => {"muc" => [1, 1, 1]}

      #			"bcub" => [6/6, 6/6, 1],
      #			"ceafm" => [1, 1, 1],
      #			"ceafe" => [1, 1, 1],
      #			"blanc" => [1, 1, 1] }
  }
);

1;
