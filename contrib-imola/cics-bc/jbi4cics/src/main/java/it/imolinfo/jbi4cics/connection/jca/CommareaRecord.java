/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
/**
 * 
 */
package it.imolinfo.jbi4cics.connection.jca;

import it.imolinfo.jbi4cics.jbi.Messages;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.resource.cci.Record;
import javax.resource.cci.Streamable;

/**
 * @author raffaele
 *
 */
public class CommareaRecord implements Streamable, Record {

  /**
   * 
   */
  private static final long serialVersionUID = -4083607545259706340L;
  
  /**
   * The responsible to translate localized messages.
   */
  private static final Messages MESSAGES
          = Messages.getMessages(CommareaRecord.class);
  
  /**
   * 
   */
  private String recordName;
  private String recordShortDescription;
  private byte[] commarea;

  /**
   * 
   */
  public CommareaRecord() {
    super();
    // TODO Auto-generated constructor stub
  }
  
  /**
   * @return Returns the commareaLength.
   */
  public int getCommareaLength() {
    return commarea.length;
  }

  /**
   * @param commareaLength The commareaLength to set.
   */
  public void setCommareaLength(int commareaLength) {
    commarea=new byte[commareaLength];
  }

  

  /* (non-Javadoc)
   * @see javax.resource.cci.Streamable#read(java.io.InputStream)
   */
  public void read(InputStream is) throws IOException {
    int byteRead=is.read(commarea);
    if (byteRead!=commarea.length){
      throw new IOException(MESSAGES.getString("CIC000301_IO_exception", new Object[] {byteRead, commarea.length})); 
    }    
  }

  /* (non-Javadoc)
   * @see javax.resource.cci.Streamable#write(java.io.OutputStream)
   */
  public void write(OutputStream os) throws IOException {
    os.write(commarea);
  }

  /* (non-Javadoc)
   * @see javax.resource.cci.Record#getRecordName()
   */
  public String getRecordName() {
    return recordName;
  }

  /* (non-Javadoc)
   * @see javax.resource.cci.Record#setRecordName(java.lang.String)
   */
  public void setRecordName(String arg0) {
    recordName=arg0;
  }

  /* (non-Javadoc)
   * @see javax.resource.cci.Record#setRecordShortDescription(java.lang.String)
   */
  public void setRecordShortDescription(String arg0) {
    recordShortDescription=arg0;
  }

  /* (non-Javadoc)
   * @see javax.resource.cci.Record#getRecordShortDescription()
   */
  public String getRecordShortDescription() {
    return recordShortDescription;
  }

  /**
   * @return Returns the commarea.
   */
  public byte[] getCommarea() {
    return commarea;
  }

  /**
   * @param commarea The commarea to set.
   */
  public void setCommarea(byte[] commarea) {
    this.commarea = commarea;
  }
  
  public Object clone(){
    CommareaRecord cloned=new CommareaRecord();
    cloned.setRecordName(recordName);
    cloned.setRecordShortDescription(recordShortDescription);
    byte[] buffer=new byte[commarea.length];
    System.arraycopy(commarea,0,buffer,0,commarea.length);
    cloned.setCommarea(buffer);
    return cloned;
  }

}

/**
 * la stessa classe in spring, può dare qualche suggerimento...
 */

