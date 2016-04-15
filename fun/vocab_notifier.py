import datetime
import sys

start_date = datetime.date(2016, 3, 2)
if len(sys.argv) == 1:
    curr_date = datetime.date.today()
elif sys.argv[1] == "-h" or sys.argv[1] == "--help":
    print("""Usage: vocab_notifier <date>
             date format: "dd-mm-yyyy"
          """)
    exit()
else:
    curr_date = datetime.datetime.strptime(sys.argv[1], "%d-%m-%Y").date()

revision_days = [1, 4, 11, 31, 61, 106, 166]

flag = 0
email_body = "Today you need to revise vocabulary of the following dates: \n"
for r in revision_days:
    revise = curr_date - datetime.timedelta(days=r)
    if(revise >= start_date):
        flag = 1
        email_body += "- " + revise.strftime("%d-%m-%Y") + "\n"


def send_email(email_body):
    email_subject = curr_date.strftime() + " Vocab REVISIONS"

# Prints to stdout for the time being
if flag:
    print(email_body)
