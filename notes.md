This file is just to point out some trick and small insight.

- To count the number of emails in a certain folder ("sent" for example):
`find ../enron_mail_20150507 -name "sent" | xargs ls -l | grep -v sent | grep -v total | wc -l`
-> 58257
