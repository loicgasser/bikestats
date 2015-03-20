import csv
import psycopg2

maintenance = ['5001', '5002', '5003', '5004', '5005', '5006', '5007', '5008', '5009', '5010', '5011', '5012', '5013', '5014', '5015', '5016', '5017', '5018', '5019', '1253', '2491', '2492', '2493', '2494', '2495', '2496', '2497' '2498', '2499']

dirt = ['-', '0', 'progress', 'multiple']

def getMonthNumberFromString(nb):
    if nb == 'Gen':
        return '01'
    elif nb == 'Feb':
        return '02'
    elif nb == 'Mar':
        return '03'
    elif nb == 'Apr':
        return '04'
    elif nb == 'Mag':
        return '05'
    elif nb == 'Giu':
        return '06'
    elif nb == 'Lug':
        return '07'
    elif nb == 'Ago':
        return '08'
    elif nb == 'Set':
        return '09'
    elif nb == 'Ott':
        return '10'
    elif nb == 'Nov':
        return '11'
    elif nb == 'Dic':
        return '12'


with open('transactions_lausanne-morges_2010/transactions_gap2_2010.csv', 'rb') as csvfile:
    file_csv = csv.reader(csvfile, delimiter=';')
    headers = list()
    temporary = list()
    counter = 0
    for row in file_csv:
        temp = dict()
    	if counter == 0:
            for entry in row:
                headers.append(entry)
        elif row[1] not in dirt and row[3] not in dirt:
	        # Sometimes there is a space at the begging of the string
            user_id = row[0]
            from_station = row[2].split(' ')[0][1:] if row[2].split(' ')[0] != '' else row[2].split(' ')[1][1:]
            to_station = row[4].split(' ')[0][1:] if row[4].split(' ')[0] != '' else row[4].split(' ')[1][1:]
            from_timestamp_time = row[1].split('-')[1].split(' ')[1]
            to_timestamp_time = row[3].split('-')[1].split(' ')[1]
            from_timestamp_day = row[1].split('-')[0].split(' ')[0]
            to_timestamp_day = row[3].split('-')[0].split(' ')[0]
            from_timestamp_month = getMonthNumberFromString(row[1].split('-')[0].split(' ')[1])
            to_timestamp_month = getMonthNumberFromString(row[3].split('-')[0].split(' ')[1])
        
            from_timestamp = '2010-' + from_timestamp_month + '-' + from_timestamp_day + ' ' + from_timestamp_time
            to_timestamp = '2010-' + to_timestamp_month + '-' + to_timestamp_day + ' ' + to_timestamp_time 
	        # Because there are missing values in the excel sheet
            if from_station != '' and to_station != '' and user_id not in maintenance:
                temp['user_id'] = user_id
                temp['from_station'] = from_station
                temp['to_station'] = to_station
                temp['from_timestamp'] = from_timestamp
                temp['to_timestamp'] = to_timestamp
                temporary.append(temp)
            else:
                print user_id
        counter += 1

## Update DB table already created
conn = psycopg2.connect("dbname=velopass user=loic")
cur = conn.cursor()

for row in temporary:
    query = ('''INSERT INTO transactions (user_id, from_station, to_station, from_timestamp, to_timestamp) VALUES (%s, %s, %s, TIMESTAMP '%s', TIMESTAMP '%s')''') % (row['user_id'], row['from_station'], row['to_station'], row['from_timestamp'], row['to_timestamp'])
    cur.execute(query)

conn.commit()
