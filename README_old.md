[Dan Garrette]: http://cs.utexas.edu/~dhg
[Jason Baldridge]: http://www.jasonbaldridge.com
[Jason Mielens]: http://jason.mielens.com/


# Low-Resource POS-Tagging: 2013

**Author:** Dan Garrette (dhg@cs.utexas.edu)



This is the code used in the papers

> [Learning a Part-of-Speech Tagger from Two Hours of Annotation](http://www.cs.utexas.edu/users/dhg/papers/garrette_baldridge_naacl2013.pdf)  
> [Dan Garrette] and [Jason Baldridge]  
> In Proceedings of NAACL 2013  

> [Real-World Semi-Supervised Learning of POS-Taggers for Low-Resource Languages](http://www.cs.utexas.edu/users/dhg/papers/garrette_mielens_baldridge_acl2013.pdf)  
> [Dan Garrette], [Jason Mielens], and [Jason Baldridge]  
> In Proceedings of ACL 2013  


This archive contains code, written in Scala, for training and tagging using the approach described in the papers.
You do not need to have Scala installed in order to run the software since Scala runs on the Java Virtual Machine (JVM).
Thus, if you have Java installed, you should be able to run the system as described below.

The archive also contains training data, testing data, and annotations for Malagasy, as used in the papers.


This code is frozen as of the version used to obtain the results in the paper. It will not be maintained.


## Setting things up


### Getting the code

Clone the project:

    $ git clone https://github.com/dhgarrette/low-resource-pos-tagging-2013.git
    
    
The rest of these instructions assume starting from the `low-resource-pos-tagging-2013` directory.


### Compile the project

Simply run

    $ ./compile
    
This will create an executable called `run`.


### Setting up Foma for FSTs

Download the most recent version (0.9.17 as of this writing) of Foma from 
http://code.google.com/p/foma/downloads/list and follow the instructions to install.

Set an environment variable for the directory where Foma is installed:

    $ export FOMA_HOME="/path/to/foma"
    
The directory at `FOMA_HOME` should contain the `flookup` executable:

    $ ls $FOMA_HOME
    flookup
    ...

Foma can be tested using an FST:

    $ echo "mananika" | $FOMA_HOME/flookup data/mlg/fst/mlg-20.fst
    mananika    +V+PRES+anika


## Running the system

Note: You should set the `JAVA_OPTS` environment variable to increase the available memory:

    export JAVA_OPTS="-Xmx4g"
    
    
### Training a tagger

    $ ./train PARAMETERS
    
The command line parameters are given below:

    COMMAND LINE PARAMETERS
    
    -rawDataFile FILEPATH ...
    Files of raw sentences.  Each file should be tokenized, with space-separated
    tokens, and one sentence per line.

    -rawDataNumTokens NUMBER
    Number of tokens to use from the raw data.  Token counting starts at the top of
    the first raw file and stops after the last complete sentence such that the total
    number of tokens is less than or equal to NUMBER.

    -typeAnnotationFile FILEPATH ..., default: not used
    Files of type-supervised words.  The contents should be whitespace-separated 
    WORD|TAG pairs.

    -tokenAnnotationFile FILEPATH ..., default: not used
    Files of token-supervised sentences.  Each line should contain a tokenized sentence,
    with tokens separated by whitespace, and each token paired with its POS tag in the
    form: WORD|TAG.

    -useAffixes (true|false), default: true
    Use affix features in the LP graph?

    -fstFile FILEPATH (foma|xfst), default: not used
    Point to either a Foma or XFST compile FST file and specify the application.

    -externalDictFile FILEPATH ..., default: not used
    Any external dictionary that should be used in the LP graph.  File should be
    a simple text file with words in the first column, each followed by one or more
    potential POS tags.

    -useModelMin (true|false), default: true
    Use the model minimization procedure after running label propagation?

    -evaluationDataFile FILEPATH ..., default: not used
    Files of annotated sentences for evaluation.  Each line should contain a tokenized
    sentence, with tokens separated by whitespace, and each token paired with its POS
    tag in the form: WORD|TAG.

    -outputModelFile FILEPATH, default: not used
    This is the location to which the trained model will be saved.


### Tagging

Run a saved tagger on a file of raw sentences.
Tagged output is written to stdout.

    $ ./tag PARAMETERS
    
The command line parameters are given below:

    COMMAND LINE PARAMETERS
    
    -taggerModelFile FILEPATH
    The location of the trained tagger

    -fileToTag FILEPATH ...
    The locations of tokenized files, one space-separated sentence per line
    that should be tagged.


## Training and using a model 

This example uses the Malagasy data found in `data/mlg`

    ./train -rawDataFile data/mlg/raw.txt -rawDataNumTokens 500000 -typeAnnotationFile data/mlg/train/mlg-tagdict-4hr-lc8.txt -tokenAnnotationFile data/mlg/train/mlg-sentences-4hr-lc8.txt -useAffixes true -fstFile data/mlg/fst/mlg-20.fst foma -externalDictFile data/mlg/dict.txt -useModelMin true -evaluationDataFile data/mlg/test.txt -outputModelFile mlg.model
    ./tag -taggerModelFile mlg.model -fileToTag FILES

See the *trained models* [README](https://github.com/dhgarrette/low-resource-pos-tagging-2013/tree/master/data/trained_models) for more information.

