package org.apache.commons.net.ftp.ssl;

public interface ReadWriteHandler {  
  public void handleRead();
  public void handleWrite();  
}