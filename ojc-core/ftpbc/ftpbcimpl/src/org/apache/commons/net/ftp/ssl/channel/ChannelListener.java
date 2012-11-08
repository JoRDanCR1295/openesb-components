package org.apache.commons.net.ftp.ssl.channel;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;


public interface ChannelListener {
  /**
   * Invoked when the underlying socket is ready to be read
   * for application data
   */
  public void processInbound();
  /**
   * Invoked when the underlying socket is ready to be written
   * for application data
   */  
  public void processOutbound();
  /**
   * inject the wrapped secure channel
   * @param channel
   */
  public void setSecureChannel(Channel channel);
  public InputStream getInputStream();
  public OutputStream getOutputStream();
  public boolean queueOutput(ByteBuffer buffer) throws IOException;
  public void getInput() throws IOException;
  public void close();
  public void notifySSLChannelClosure();
  public void waitSSLChannelClosure();
}
