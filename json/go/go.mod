module benchmark

go 1.21

require (
	benchmarks/common v0.0.0-00010101000000-000000000000
	github.com/bytedance/sonic v1.11.9
	github.com/goccy/go-json v0.10.3
	github.com/json-iterator/go v1.1.12
	github.com/willabides/rjson v0.2.0
)

require (
	github.com/bytedance/sonic/loader v0.1.1 // indirect
	github.com/cloudwego/base64x v0.1.4 // indirect
	github.com/cloudwego/iasm v0.2.0 // indirect
	github.com/klauspost/cpuid/v2 v2.2.8 // indirect
	github.com/modern-go/concurrent v0.0.0-20180306012644-bacd9c7ef1dd // indirect
	github.com/modern-go/reflect2 v1.0.2 // indirect
	github.com/twitchyliquid64/golang-asm v0.15.1 // indirect
	golang.org/x/arch v0.8.0 // indirect
	golang.org/x/sys v0.22.0 // indirect
)

replace benchmarks/common => ../../common/go
