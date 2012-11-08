/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)RepositoryDirectories.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.common.registry;

import java.io.*;

import com.sun.stc.common.utils.StdErr;
import com.sun.stc.common.utils.StdOut;

/**
 * This type was created in VisualAge.
 */
public class RepositoryDirectories implements RegistryConstants
{

	private String sharedExe = "";
	private String logs = "";
	private String iqueueData = "";
	private String iqueueIndex = "";
	private String systemData = "";

/**
 * RepositoryDirectories constructor comment.
 */
public RepositoryDirectories()
{
	
	super();
}
/**
 * This method was created in VisualAge.
 * @return java.lang.String
 * @param name java.lang.String
 */
public String extractPath(String file, String name) 
{
	char c;
	int pos;
  int pos2;
  String return_value;
	
	pos = file.indexOf(name);
	pos += name.length();
	
	pos2 = pos;
				
	c = file.charAt(pos);
	while (Character.isWhitespace(c) == false)
  {
		pos2 ++;
		c = file.charAt(pos2);
	}
  
  return_value = file.substring(pos, pos2);
// System.out.println(return_value);	
	return (return_value);
}
protected void finalize() throws Throwable
{
	//	System.out.println("Finalizing RepositoryDirectories...");
	sharedExe = null;
	logs = null;
	iqueueData = null;
	iqueueIndex = null;
	systemData = null;
	super.finalize();
}
/**
 * This method was created in VisualAge.
 * @return java.lang.String
 */
public String getIqueueData() {
	return iqueueData;
}
/**
 * This method was created in VisualAge.
 * @return java.lang.String
 */
public String getIqueueIndex() {
	return iqueueIndex;
}
/**
 * This method was created in VisualAge.
 * @return java.lang.String
 */
public String getLogs() {
	return logs;
}
/**
 * This method was created in VisualAge.
 * @return int
 */
public String getSharedExe() {
	return sharedExe;
}
/**
 * This method was created in VisualAge.
 * @return java.lang.String
 */
public String getSystemData() {
	return systemData;
}
public static void main(String[] args)
{
  for (int i = 0; i < args.length; i++)
  {
	if (args[i].startsWith("-d", 0))
	{
	  StdOut.setDebugEnabled(true);
	}
  }

  RepositoryDirectories rpd = new RepositoryDirectories();

  if (rpd.readRepositoryDirectories())
  {
	System.err.println("System Data: " + rpd.getSystemData());
	System.err.println("IQueueIndex: " + rpd.getIqueueIndex());
	System.err.println("IQueueData:  " + rpd.getIqueueData());
	System.err.println("Logs:        " + rpd.getLogs());
	System.err.println("SharedExe:   " + rpd.getSharedExe());
  }
  else
  {
	System.err.println("Cannot read repository directories");
  }
}
/**
 * This method was created in VisualAge.
 * @return boolean
 */
public boolean readRepositoryDirectories()
{
	byte[] file_bytes;
	int file_size;
	String entire_file;
	String temp_string;
	String shared;
	String logs;
	String iq_data;
	String iq_index;
	String system;
	File file;
	DataInputStream input_stream;
	FileInputStream file_stream;
		String home_dir = this.home_dir;
                
		//esr 66138
		if (System.getProperty("egst") != null) {
		    home_dir = System.getProperty("egst");
		    System.out.println("home_dir=" + home_dir);
		}	   
		String egateStore = ".egate.store";
		if (System.getProperty("STCPRODUCT_MAIN") != null) {
		    egateStore = "." + System.getProperty("STCPRODUCT_MAIN") + ".store";
		    System.out.println("egateStore=" + egateStore);
		}	   

		if (home_dir.equals("")) 
		{
		   if (System.getProperty("os.name").startsWith("Win"))
		   {
			 home_dir = "c:/";
		   }
		   else
		   {
			 home_dir = "~/";
		   }
		}
        
	//esr 66138
	//file = new File(home_dir, ".egate.store");
	file = new File(home_dir, egateStore);

	if (file.exists() == false)
	{
	  //
	  //if file does not exist in user.home,
	  //default home_dir to c:/ and check there
	  //
	  home_dir = "c:/";
	  //esr 66138
	  //file = new File(home_dir, ".egate.store"); 
	  file = new File(home_dir, egateStore); 
	}

 if (home_dir != null)
 {
   StdOut.println("home dir is " + home_dir);
 }

	if (file.exists())
	{
		if (file.isFile() && file.canRead())
		{
			try
			{
				file_stream = new FileInputStream(file);
				input_stream = new DataInputStream(file_stream);

				file_size = (int) file.length();
				file_bytes = new byte[file_size];
				input_stream.read(file_bytes);
				entire_file = new String (file_bytes);

				shared = extractPath(entire_file, "SharedExe=");
				logs = extractPath(entire_file, "Logs=");
				iq_data = extractPath(entire_file, "IQueueData=");
				iq_index = extractPath(entire_file, "IQueueIndex=");
				system = extractPath(entire_file, "SystemData=");

				if (shared == null || logs == null || iq_data == null || iq_index == null || system == null)
				  return (false);

				this.setSharedExe(shared);
				this.setLogs(logs);
				this.setIqueueData(iq_data);
				this.setIqueueIndex(iq_index);
				this.setSystemData(system);
				
			}
			catch (FileNotFoundException e)
			{
				StdErr.println("file not found.");
				return (false);
			}
			catch (IOException e)
			{
				StdErr.println("error reading file from input stream.");
				return (false);
			}
		}
		return (true);
	} // end of file.exists()
	return (false);
}
/**
 * This method was created in VisualAge.
 * @param newValue java.lang.String
 */
public void setIqueueData(String newValue) {
	this.iqueueData = newValue;
}
/**
 * This method was created in VisualAge.
 * @param newValue java.lang.String
 */
public void setIqueueIndex(String newValue) {
	this.iqueueIndex = newValue;
}
/**
 * This method was created in VisualAge.
 * @param newValue java.lang.String
 */
public void setLogs(String newValue) {
	this.logs = newValue;
}
/**
 * This method was created in VisualAge.
 * @return boolean
 */
public boolean setRepositoryDirectories() 
{
  String slash;
  
	if (sharedExe == null)
	sharedExe = new String();
  if (systemData == null)
	systemData = new String();
  if (logs == null)
	logs = new String();
  if (iqueueData == null)
	iqueueData = new String();
  if (iqueueIndex == null)
	iqueueIndex = new String();

  if (sharedExe.equals("")  && systemData.equals(""))
	  return (false);

	if (systemData.equals(""))
		systemData = new String(sharedExe);
	else
	  sharedExe = new String(systemData);

	slash = this.separator;
	logs = new String(systemData + slash + "logs");
	iqueueData = new String(systemData + slash + "iq");
	iqueueIndex = new String(systemData + slash + "iq");
	
	
	
	
	return false;
}
/**
 * This method was created in VisualAge.
 * @param newValue int
 */
public void setSharedExe(String newValue) {
	this.sharedExe = newValue;
}
/**
 * This method was created in VisualAge.
 * @param newValue java.lang.String
 */
public void setSystemData(String newValue) {
	this.systemData = newValue;
}
}
