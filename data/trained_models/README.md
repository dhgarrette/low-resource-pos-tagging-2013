# Trained POS-Tagging Models

## Kinyarwanda

    data/trained_models/kin.model

Command:

    ./train -rawDataFile data/kin/raw.txt -rawDataNumTokens 500000 -typeAnnotationFile data/kin/train/kin-tagdict-4hr-lc8.txt -tokenAnnotationFile data/kin/train/kin-sentences-4hr-lc8.txt -useAffixes true -fstFile data/kin/fst/kin-20.fst foma -externalDictFile  -useModelMin true -evaluationDataFile data/kin/test.txt -outputModelFile data/trained_models/kin.model

Training information:

	Raw data: 13281 sentences, 206492 tokens
	Type annoations: 3682 tagdict entries, P=0.22, R=0.36  (4 hours annotation time)
	Token annotations: 67 sentences, 4153 tokens, 1679 tagdict entries, P=0.26, R=0.19  (4 hours annotation time)
	LP features include affixes (up to 5 characters) and morphological features produced from an FST built in 10 hours.

Test set evaluation:

	input tag dict precision  = 2.44
	input tag dict recall     = 55.92
	output tag dict precision = 78.77
	output tag dict recall    = 75.02

    Total:   80.71 (4213/5220)
	Known:   82.68 (3126/3781)
	Unknown: 75.54 (1087/1439)
	Common Mistakes:
	#Err     Gold      Model
	185      V        N
	110      CC       PREP
	72       ADV      N
	71       N        V
	66       ADJ      V

Tagging new data:

    ./tag -taggerModelFile data/trained_models/kin.model -fileToTag FILES


## Malagasy

    data/trained_models/mlg.model

Command:

	./train -rawDataFile data/mlg/raw.txt -rawDataNumTokens 500000 -typeAnnotationFile data/mlg/train/mlg-tagdict-4hr-lc8.txt -tokenAnnotationFile data/mlg/train/mlg-sentences-4hr-lc8.txt -useAffixes true -fstFile data/mlg/fst/mlg-20.fst foma -externalDictFile data/mlg/dict.txt -useModelMin true -evaluationDataFile data/mlg/test.txt -outputModelFile data/trained_models/mlg.model

Training information:

	Raw data: 28216 sentences, 499965 tokens
	Type annoations: 2773 tagdict entries, P=0.16, R=0.26  (4 hours annotation time)
	Token annotations: 176 sentences, 4230 tokens, 1381 tagdict entries, P=0.30, R=0.25  (4 hours annotation time)
	LP features include affixes (up to 5 characters), a morphological features produced from an FST built in 10 hours, and the Malagasy World Dictionary.

Test set evaluation:

	input tag dict precision  = 2.09
	input tag dict recall     = 50.74
	output tag dict precision = 76.27
	output tag dict recall    = 76.99

	Total:   83.24 (4415/5304)
	Known:   85.76 (3379/3940)
	Unknown: 75.95 (1036/1364)
	Common Mistakes:
	#Err     Gold      Model
	81       PN       N
	58       ADJ      N
	56       PCL      CONJ
	56       DT       PRO
	45       V        N

Tagging new data:

    ./tag -taggerModelFile data/trained_models/mlg.model -fileToTag FILES

