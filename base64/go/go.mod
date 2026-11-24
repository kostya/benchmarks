module benchmark

go 1.26.0

require (
	benchmarks/common v0.0.0-00010101000000-000000000000
	github.com/chenzhuoyu/base64x v0.0.0-20230717121745-296ad89f973d
)

require (
	github.com/bytedance/sonic/loader v0.5.3 // indirect
	github.com/klauspost/cpuid/v2 v2.4.2 // indirect
	golang.org/x/sys v0.50.0 // indirect
)

replace benchmarks/common => ../../common/go
