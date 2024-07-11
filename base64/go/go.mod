module benchmark

go 1.21

require (
	benchmarks/common v0.0.0-00010101000000-000000000000
	github.com/chenzhuoyu/base64x v0.0.0-20230717121745-296ad89f973d
)

require (
	github.com/bytedance/sonic/loader v0.1.1 // indirect
	github.com/cloudwego/iasm v0.2.0 // indirect
	github.com/klauspost/cpuid/v2 v2.2.8 // indirect
	golang.org/x/sys v0.22.0 // indirect
)

replace benchmarks/common => ../../common/go
