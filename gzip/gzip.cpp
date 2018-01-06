#include <string>
#include <cstring>
#include <iostream>
#include <cstdio>
 
#include <zlib.h>
 
/* Compress gzip data */
/* data 原数据 ndata 原数据长度 zdata 压缩后数据 nzdata 压缩后长度 */
int gzipCompress(const std::string& rawData, std::string& compressedData)
{

    Bytef *data = (Bytef*)rawData.c_str();
    uLong ndata = (uLong)rawData.size(); 

    uLong nzdata = ndata*10;
    compressedData.resize(nzdata);
    Bytef *zdata = (Bytef*)compressedData.c_str();

    z_stream c_stream;
    int err = 0;

    if(data && ndata > 0) 
    {
        c_stream.zalloc = NULL;
        c_stream.zfree = NULL;
        c_stream.opaque = NULL;

        //只有设置为MAX_WBITS + 16才能在在压缩文本中带header和trailer
        if(deflateInit2(&c_stream, Z_DEFAULT_COMPRESSION, Z_DEFLATED,
                    MAX_WBITS + 16, 8, Z_DEFAULT_STRATEGY) != Z_OK) 
        {
            return -1;
        }

        c_stream.next_in  = data;
        c_stream.avail_in  = ndata;
        c_stream.next_out = zdata;
        c_stream.avail_out  = nzdata;
        while(c_stream.avail_in != 0 && c_stream.total_out < nzdata) 
        {
            if(deflate(&c_stream, Z_NO_FLUSH) != Z_OK) 
            {
                return -2;
            }
        }

        if(c_stream.avail_in != 0)
        {
            return c_stream.avail_in;
        }

        for(;;) 
        {
            if((err = deflate(&c_stream, Z_FINISH)) == Z_STREAM_END) 
                break;
            if(err != Z_OK)
            {
                return -3;
            }
        }

        if(deflateEnd(&c_stream) != Z_OK) 
        {
            return -4;
        }

        compressedData.resize(c_stream.total_out);

        return 0;
    }

    return -6;
}
 
/* Uncompress gzip data */
/* zdata 数据 nzdata 原数据长度 data 解压后数据 ndata 解压后长度 */
int gzipDecompress(const std::string& compressedData, std::string& decompressedData)
{
    Byte *zdata = (Byte*)compressedData.c_str();
    uLong nzdata = (uLong)compressedData.size();

    uLong ndata = nzdata * 8;
    decompressedData.resize(ndata);
    Byte *data = (Byte*)decompressedData.c_str();

    int err = 0;
    z_stream d_stream = {0}; /* decompression stream */
    static char dummy_head[2] = {
        0x8 + 0x7 * 0x10,
        (((0x8 + 0x7 * 0x10) * 0x100 + 30) / 31 * 31) & 0xFF,
    };

    d_stream.zalloc = NULL;
    d_stream.zfree = NULL;
    d_stream.opaque = NULL;
    d_stream.next_in = zdata;
    d_stream.avail_in = 0;
    d_stream.next_out = data;

    //只有设置为MAX_WBITS + 16才能在解压带header和trailer的文本
    if(inflateInit2(&d_stream, MAX_WBITS + 16) != Z_OK) 
    {
        return -1;
    }

    //if(inflateInit2(&d_stream, 47) != Z_OK) return -1;
    while(d_stream.total_out < ndata && d_stream.total_in < nzdata) 
    {
        d_stream.avail_in = d_stream.avail_out = 1; /* force small buffers */
        if((err = inflate(&d_stream, Z_NO_FLUSH)) == Z_STREAM_END) 
        {
            break;
        }
        if(err != Z_OK) 
        {
            if(err == Z_DATA_ERROR) 
            {
                d_stream.next_in = (Bytef*) dummy_head;
                d_stream.avail_in = sizeof(dummy_head);
                if((err = inflate(&d_stream, Z_NO_FLUSH)) != Z_OK) 
                {
                    return -1;
                }
            } 
            else 
            {
                return -1;
            }
        }
    }
    decompressedData.resize(d_stream.total_out);

    if(inflateEnd(&d_stream) != Z_OK) 
    {
        return -1;
    }
    return 0;
}
 
int main()
{
#if 0
    std::string str = "1865434534,1234352342";
    std::string compressed;
    int ret = gzipCompress(str, compressed);
    if (ret < 0)
    {
        std::cout << "compress failed! ret:" << ret << std::endl;
        return -1;
    }

    for (int i = 0; i < compressed.size(); ++i)
    {
        printf("%02x", compressed[i]&0xff);
    }
    printf("\n");
#endif

#if 1
    char buf[] = { 0x1f, 0x8b, 0x08, 0x00, 0xed, 0x5e, 0x4f, 0x5a, 0x00, 0x03, 0x33, 0xb4, 0x30, 0x33, 0x35, 0x31, 0x36, 0x31, 0x35, 0x36, 0xd1, 0x31, 0x34, 0x32, 0x36, 0x31, 0x36, 0x05, 0x12, 0x46, 0x00, 0x2b, 0xc2, 0xcd, 0xb4, 0x15, 0x00, 0x00, 0x00};
    std::string compressed(buf, sizeof(buf)/sizeof(buf[0]));

    std::string decompressed;
    if (gzipDecompress(compressed, decompressed) != 0)
    {
        std::cout << "decompress failed!" << std::endl;
        return -2;
    }

    std::cout << decompressed << std::endl;
#endif

    return 0;
}
