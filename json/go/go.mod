module benchmark

go 1.26.0

require (
	benchmarks/common v0.0.0-00010101000000-000000000000
	github.com/bytedance/sonic v1.15.5
	github.com/goccy/go-json v0.12.0
	github.com/json-iterator/go v1.1.12
	github.com/willabides/rjson v0.2.0
)

require (
	github.com/bytedance/gopkg v0.1.4 // indirect
	github.com/bytedance/sonic/loader v0.5.3 // indirect
	github.com/cloudwego/base64x v0.1.7 // indirect
	github.com/klauspost/cpuid/v2 v2.4.2 // indirect
	github.com/modern-go/concurrent v0.0.0-20180306012644-bacd9c7ef1dd // indirect
	github.com/modern-go/reflect2 v1.0.2 // indirect
	github.com/twitchyliquid64/golang-asm v0.15.1 // indirect
	golang.org/x/arch v0.33.0 // indirect
	golang.org/x/sys v0.50.0 // indirect
)

replace benchmarks/common => ../../common/go
