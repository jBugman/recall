package main

import (
	"errors"
	"fmt"
	"net"
	"strings"
	"time"

	log "gopkg.in/inconshreveable/log15.v2"
)

func listen(port string) error {
	listener, err := net.Listen("tcp", port)
	if err != nil {
		return err
	}
	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Error("accept", "err", err)
			continue
		}
		go func() {
			err = handleConnection(conn)
			if err != nil {
				log.Error("handle connection", "err", err)
			}
		}()
	}
}

func handleConnection(conn net.Conn) error {
	log.Debug("connection", "addr", conn.RemoteAddr())

	buf := make([]byte, 4096)
	n, err := conn.Read(buf)
	if err != nil {
		return err
	}
	log.Debug("recieved", "bytes", n)
	contents := string(buf[:n])
	log.Debug("request", "string", contents)
	req, err := parseRequest(contents)
	if err != nil {
		return err
	}

	log.Info("request", "method", req.method, "uri", req.uri, "body", req.body)

	resp := handleRequest(req)
	conn.Write(resp)

	log.Info("closing connection")
	return conn.Close()
}

type request struct {
	method  string
	uri     string
	headers map[string]string
	body    string
}

func parseRequest(req string) (request, error) {
	lines := strings.Split(req, "\r\n")
	requestLine := strings.Fields(lines[0])
	if len(requestLine) != 3 {
		return request{}, errors.New("Broken request-line")
	}
	r := request{
		method:  requestLine[0],
		uri:     requestLine[1],
		headers: make(map[string]string),
	}
	var i int
	for i = 1; i < len(lines); i++ {
		if lines[i] == "" {
			break
		}
		parts := strings.SplitN(lines[i], ": ", 2)
		if len(parts) != 2 {
			return request{}, errors.New("Broken header line: " + lines[i])
		}
		r.headers[parts[0]] = parts[1]
	}
	if i < len(lines) {
		r.body = strings.TrimSpace(strings.Join(lines[i:], "\r\n"))
	}
	return r, nil
}

func handleRequest(r request) []byte {
	var result string
	switch r.method {
	case "GET":
		switch r.uri {
		case "/test":
			result = makeResponse("200 OK", "text/plain; charset=utf-8", "Hello, http!")
		default:
			result = makeResponse("404 Not Found", "", "")
		}
	case "POST":
		switch r.uri {
		case "/json":
			if r.headers["Content-Type"] != "application/x-www-form-urlencoded" {
				result = makeResponse("400 Bad Request", "", "")
				break
			}
			data := parseForm(r)
			log.Debug("form", "data", data)
			if name, ok := data["name"]; !ok {
				result = makeResponse("204 No Content", "", "")
			} else {
				json := "{\"hello\": \"" + name + "\"}"
				result = makeResponse("200 OK", "applicaton/json; charset=utf-8", json)
			}
		default:
			result = makeResponse("404 Not Found", "", "")
		}
	case "DELETE":
		result = makeResponse("405 Method Not Allowed", "", "")
	default:
		result = makeResponse("501 Not Implemented", "", "")
	}
	return []byte(result)
}

var respTemplate = strings.Replace(`HTTP/1.1 %s
Date: %s
Connection: close
Server: AWSUM/0.0.1pre-alpha`,
	"\n", "\r\n", -1)

var contentTemplate = strings.Replace(`Content-Type: %s
Content-Length: %d
Last-Modified: %s

%s`,
	"\n", "\r\n", -1)

func makeResponse(code, contentType, content string) string {
	t := time.Now().Format(time.RFC1123)
	result := fmt.Sprintf(respTemplate, code, t)
	if contentType != "" {
		result += "\r\n" + fmt.Sprintf(contentTemplate, contentType, len(content), t, content)
	}
	return result
}

func parseForm(r request) map[string]string {
	result := make(map[string]string)
	if r.body == "" {
		return result
	}
	lines := strings.Split(r.body, "\r\n")
	for _, line := range lines {
		if line == "" {
			continue
		}
		parts := strings.SplitN(line, "=", 2)
		result[parts[0]] = parts[1]
	}
	return result
}

func main() {
	h := log.CallerFileHandler(log.StdoutHandler)
	log.Root().SetHandler(h)

	if err := listen(":8080"); err != nil {
		log.Crit("listen", "err", err)
	}
}
