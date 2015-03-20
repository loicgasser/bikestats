import csv
import psycopg2

'''conn = psycopg2.connect("dbname=velopass user=postgres password=")

SELECT TIMESTAMP '1999-01-08 04:05:06'

cur = conn.cursor()

cur.execute("SELECT * from public.velopass")

results = cur.fetchall()

INSERT INTO transactions_aggregated
(  
  no,
  date,
  "in",
  "out",
  diff,
  weekday,
  precipitation,
  temperature,
  wind,
  sun
)
VALUES
(
  101,
  TIMESTAMP '1999-01-08 04:05:06',
  2,
  2,
  3,
  'Mardi',
  0.2,
  0.2,
  0.2,
  0.3
)

print results'''


with open('../../shared/DesignProject/v4m.csv', 'rb') as csvfile:
    file_csv = csv.reader(csvfile, delimiter=';')
    table_entries = list()
    headers = list()
    counter = 0
    for row in file_csv:
        if counter == 0:
            for entry in row:
                headers.append(entry)
        else:
            weekday = str(row[0])
            date = str(row[1]).replace('.','-')
            hour = str(row[2]) if len(row[2]) == 2 else '0' + str(row[2])
            temperature = row[len(headers)-4]
            precipitation = row[len(headers)-3]
            wind = row[len(headers)-2]
            sun = row[len(headers)-1]
            
            i = 3
            for no in headers[3:len(headers)-4]:
                in_out_diff = no.split('in')
                if len(in_out_diff) == 2:
                    temp = dict()
                    temp['no'] = in_out_diff[1]
                    temp['in'] = row[i]
                in_out_diff = no.split('out')
                if len(in_out_diff) == 2:
                    temp['out'] = row[i]
                in_out_diff = no.split('diff')
                if len(in_out_diff) == 2:
                    temp['diff'] = row[i]
		    try:
		        int(sun)
		    except:
		        sun = 0
      
                    temp['date'] = date.replace('.','-') + ' ' + hour + ':00:00'
                    temp['weekday'] = str(weekday)
                    temp['temperature'] = float(temperature)
                    temp['precipitation'] = float(precipitation)
                    temp['wind'] = float(wind)
                    temp['sun'] = int(sun)
                    table_entries.append(temp)
		i += 1
        counter += 1

## Update DB table already created
conn = psycopg2.connect("dbname=velopass user=postgres password=")
cur = conn.cursor()
 
for row in table_entries:
    query = ('''INSERT INTO transactions_aggregated (no, date, "in", "out", diff, weekday, precipitation, temperature, wind, sun) VALUES (%s, TIMESTAMP '%s', %s, %s, %s, '%s', %s, %s, %s, %s)''') % (row['no'], row['date'], row['in'], row['out'], row['diff'], row['weekday'], row['precipitation'], row['temperature'], row['wind'], row['sun'])
    cur.execute(query)
conn.commit()
