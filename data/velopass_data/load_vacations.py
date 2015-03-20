import csv
import psycopg2


with open('/home/user/shared/DesignProject/vacances.csv', 'rb') as csvfile:
    file_csv = csv.reader(csvfile, delimiter=';')
    headers = list()
    table_entries = list()
    counter = 0
    for row in file_csv:
        if counter == 0:
            for entry in row:
                headers.append(entry)
        else:
	    temp = dict()
	    temp[headers[0]] = row[0].split('/')[2] + '-' + row[0].split('/')[1] + '-' + row[0].split('/')[0] #+ '12:00:00'
	    temp[headers[1]] = False if row[1] == '0' else True 
            temp[headers[2]] = False if row[2] == '0' else True
            temp[headers[3]] = False if row[3] == '0' else True
            temp[headers[4]] = False if row[4] == '0' else True
            table_entries.append(temp)
	counter += 1

## Update DB table already created
conn = psycopg2.connect("dbname=velopass user=postgres password=")
cur = conn.cursor()

for row in table_entries:
    sql = ('''INSERT INTO vacations (date, vacances_scolaires, ic_epfl, jours_feries, vacances_epfl) VALUES (TIMESTAMP '%s', %s, %s, %s, %s)''') % (row['date'], row['vacances_scolaires'], row['ic_epfl'], row['jours_feries'], row['vacances_epfl'])
    cur.execute(sql)
conn.commit()
