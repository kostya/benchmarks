module benchmark

go 1.23.0

require (
	benchmarks/common v0.0.0-00010101000000-000000000000
	github.com/bytedance/sonic v1.14.0
	github.com/goccy/go-json v0.10.5
	github.com/json-iterator/go v1.1.12
	github.com/willabides/rjson v0.2.0
)

require (
	github.com/bytedance/sonic/loader v0.3.0 // indirect
	github.com/cloudwego/base64x v0.1.6 // indirect
	github.com/klauspost/cpuid/v2 v2.3.0 // indirect
	github.com/modern-go/concurrent v0.0.0-20180306012644-bacd9c7ef1dd // indirect
	github.com/modern-go/reflect2 v1.0.2 // indirect
	github.com/twitchyliquid64/golang-asm v0.15.1 // indirect
	golang.org/x/arch v0.20.0 // indirect
	golang.org/x/sys v0.35.0 // indirect
)

replace benchmarks/common => ../../common/go
