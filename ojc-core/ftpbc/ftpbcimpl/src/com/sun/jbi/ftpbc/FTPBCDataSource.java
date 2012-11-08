package com.sun.jbi.ftpbc;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.activation.DataSource;

public class FTPBCDataSource implements DataSource {

    private String name = null;
    private InputStream inputStream = null;

    public FTPBCDataSource(InputStream inputStream, String name) {
        this.name = name;
        this.inputStream = inputStream;
    }

    public String getContentType() {
        return "bytes";
    }

    public InputStream getInputStream() throws IOException {
        return inputStream;
    }

    public String getName() {
        return name;
    }

    public OutputStream getOutputStream() throws IOException {
        return null;
    }
}
