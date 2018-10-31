package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net"
	"net/http"
	"strings"
)

func GetPrivateIP() (privateIP string) {
	conn, err := net.Dial("udp", "8.8.8.8:80")
	if err != nil {
		fmt.Println("err Dial: ", err)
		return
	}
	defer conn.Close()

	localAddr := conn.LocalAddr().String()
	idx := strings.LastIndex(localAddr, ":")
	privateIP = localAddr[0:idx]

	return
}

func GetPublicIP() (publicIP string) {
	resp, err := http.Get("http://myexternalip.com/raw")
	if err != nil {
		fmt.Printf("get external ip from myexternlip.com error: ", err)
		return
	}
	content, err := ioutil.ReadAll(resp.Body)
	publicIP = strings.Replace(string(content), "\n", "", -1) // -1 means replace all
	return
}

type OperatorInfo struct {
	Code   int `json:"code"`
	IPInfo `json:"data"`
}

type IPInfo struct {
	Ip         string `json:"ip"`
	Country    string `json:"country"`
	Area       string `json:"area"`
	Region     string `json:"region"`
	City       string `json:"city"`
	County     string `json:"county"`
	Isp        string `json:"isp"`
	Country_id string `json:"country_id"`
	Area_id    string `json:"area_id"`
	Region_id  string `json:"region_id"`
	City_id    string `json:"city_id"`
	County_id  string `json:"county_id"`
	Isp_id     string `json:"isp_id"`
}

var (
	ErrLoopback       = errors.New("Error Loop back address!")
	ErrLocalMulticast = errors.New("Error multicast address!")
	ErrLocalUnicast   = errors.New("Error unicast address!")
	ErrPrivateAddr    = errors.New("Error private address!")
)

func IsPublicIP(dotIP string) (err error) {
	ip := net.ParseIP(dotIP)
	if ip.IsLoopback() {
		return ErrLoopback
	}
	if ip.IsLinkLocalMulticast() {
		return ErrLocalMulticast
	}
	if ip.IsLinkLocalUnicast() {
		return ErrLocalUnicast
	}

	if ip4 := ip.To4(); ip4 != nil {
		switch {
		case ip4[0] == 10:
			return ErrPrivateAddr
		case ip4[0] == 192 && ip4[1] == 168:
			return ErrPrivateAddr
		case ip4[0] == 172 && ip4[1] >= 16 && ip4[1] <= 31:
			return ErrPrivateAddr
		}
	}

	return nil
}

// Query Operator info of given ip
func GetOperatorInfo(ip string) (info string) {
	TaobaoQueryUrl := "http://ip.taobao.com/service/getIpInfo.php?ip=" + ip
	resp, err := http.Get(TaobaoQueryUrl)
	if err != nil {
		fmt.Printf("query from taobao ip tool error: ", err)
		return
	}
	content, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Println("Read taobao ip response error: ", err)
		return
	}

	var oi OperatorInfo
	if err = json.Unmarshal(content, &oi); err != nil {
		fmt.Println("Unmarshal operator info error: ", err)
		return
	}

	if oi.Code != 0 {
		fmt.Println("Ret Code not 0! check content:", string(content))
	} else {
		fmt.Println(oi.IPInfo)
	}

	return
}

func main() {

	fmt.Println(net.ParseIP("192.168.0.1"))
	addrs, err := net.InterfaceAddrs()
	if err != nil {
		fmt.Println("Error InterfaceAddrs(): ", err)
		return
	}
	for _, addr := range addrs {
		if ipNet, ok := addr.(*net.IPNet); ok && !ipNet.IP.IsLoopback() {
			fmt.Println(addr)
		}
	}
	privIP := GetPrivateIP()
	fmt.Printf("PrivateIP: %s. ", privIP)
	if err = IsPublicIP(privIP); err != nil {
		fmt.Println("It's not public IP: ", err)
	} else {
		fmt.Println("It's public IP: ", privIP)
	}

	// 通过查询myexternalip.com获取公网IP
	pubIP := GetPublicIP()
	fmt.Printf("PublicIP: %s. ", pubIP)
	// 判断是不是公网IP
	if err = IsPublicIP(pubIP); err != nil {
		fmt.Println("It's not public IP: ", err)
	} else {
		fmt.Println("It's public IP: ", pubIP)
	}

	// 获取运营商信息
	GetOperatorInfo("39.108.222.178")

}
