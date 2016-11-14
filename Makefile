main:
	stack build
	stack exec vll ~/tmp/test_csv/companies.csv | less 
