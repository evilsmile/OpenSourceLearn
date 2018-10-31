package main

import (
	"database/sql"
	"fmt"
	_ "github.com/go-sql-driver/mysql"
	"log"
)

func checkErr(err error) {
	if err != nil {
		panic(err)
	}
}

func NewMySqlWrapper(user, host, passwd, dbName string) *MySqlWrapper {

	auth := fmt.Sprintf("%s:%s@tcp(%s)/%s?charset=utf8", user, passwd, host, dbName)
	db, err := sql.Open("mysql", auth)
	checkErr(err)

	return &MySqlWrapper{db: db}
}

type DataRow struct {
	Id     int
	Color  string
	Weight int
	Brand  string
}

type MySqlWrapper struct {
	db *sql.DB
}

func (sqlW MySqlWrapper) Query(sqlcmd string) {
	if sqlW.db == nil {
		log.Fatal("nil db")
	}
	rows, err := sqlW.db.Query(sqlcmd)
	checkErr(err)

	for rows.Next() {
		columns, _ := rows.Columns()

		scanArgs := make([]interface{}, len(columns))
		values := make([]interface{}, len(columns))

		for i := range values {
			scanArgs[i] = &values[i]
		}

		err = rows.Scan(scanArgs...)
		record := make(map[string]string)
		for i, col := range values {
			if col != nil {
				record[columns[i]] = string(col.([]byte))
			}
		}
		fmt.Println(record)
	}
	rows.Close()
}

func (sqlW MySqlWrapper) Query2(sqlcmd string) (ret []DataRow) {
	if sqlW.db == nil {
		log.Fatal("nil db")
	}
	rows, err := sqlW.db.Query(sqlcmd)
	checkErr(err)

	result := make([]DataRow, 0)
	for rows.Next() {

		var dr DataRow
		if err = rows.Scan(&dr.Id, &dr.Color, &dr.Weight, &dr.Brand); err != nil {
			panic("err get row")
		}
		result = append(result, dr)
	}
	rows.Close()

	return result
}

func (sqlW MySqlWrapper) Update() (err error) {
	if sqlW.db == nil {
		log.Fatal("nil db")
	}

	stmt, err := sqlW.db.Prepare("update ian.phone_info set brand=? where id=?")
	checkErr(err)

	res, err := stmt.Exec("new Apple", 2)
	checkErr(err)
	num, err := res.RowsAffected()
	checkErr(err)

	fmt.Println("Affected: ", num)
	stmt.Close()

	return nil
}

func (sqlW MySqlWrapper) Remove() (err error) {
	if sqlW.db == nil {
		log.Fatal("nil db")
	}

	stmt, err := sqlW.db.Prepare("delete from ian.phone_info where brand=?")
	checkErr(err)

	res, err := stmt.Exec("Apple2")
	checkErr(err)
	num, err := res.RowsAffected()
	checkErr(err)

	fmt.Println("Affected: ", num)
	stmt.Close()

	return nil
}

func (sqlW MySqlWrapper) Insert() (err error) {
	if sqlW.db == nil {
		log.Fatal("nil db")
	}

	stmt, err := sqlW.db.Prepare("insert into ian.phone_info values(?, ?, ?, ?)")
	checkErr(err)

	res, err := stmt.Exec(nil, "red", 55, "Apple2")
	checkErr(err)
	id, err := res.LastInsertId()
	checkErr(err)

	fmt.Println("Inserted Id: ", id)
	stmt.Close()

	return nil
}

func main() {

	mysqlWrapper := NewMySqlWrapper("root", "172.18.22.217", "123Naruto", "ian")
	dataRow := mysqlWrapper.Query2("select id, color, weight, brand from ian.phone_info")

	for _, row := range dataRow {
		fmt.Println(row)
	}

	mysqlWrapper.Update()
	mysqlWrapper.Remove()
	mysqlWrapper.Insert()
}
