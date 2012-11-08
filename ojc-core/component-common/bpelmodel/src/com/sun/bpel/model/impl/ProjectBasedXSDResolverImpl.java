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
 * @(#)ProjectBasedXSDResolverImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.net.MalformedURLException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.ProjectBasedXSDResolver;
import com.sun.bpel.model.common.EInsightModelException;
import com.sun.bpel.xml.xsd.XMLSchema;

/**
 * 
 * @author Sun Microsystems
 *
 */
public class ProjectBasedXSDResolverImpl extends DefaultXSDResolverImpl implements ProjectBasedXSDResolver {

	private List mProjectClassPath;
	
	public static final String WSDL_FILE_EXTENSION = "wsdl";
	
	private static XSDFileFilter xsdFilter = new XSDFileFilter();
	
	private Logger mLogger = Logger.getLogger(ProjectBasedXSDResolverImpl.class.getName());
	
	public ProjectBasedXSDResolverImpl(String baseURI, BPELParseContext parseContext) throws EInsightModelException {
		super(baseURI, parseContext);
	}
	
	public void setProjectClassPath(List pClasspath) {
		this.mProjectClassPath = pClasspath;
		
	}

	public XMLSchema resolve(String publicId, String systemId) throws EInsightModelException {
		XMLSchema xmlSchema = super.resolve(publicId, systemId);
		if(xmlSchema != null) {
			return xmlSchema;
		}
		
		File resultantFile = null;
		
//		no matching file found in current project , so now look 
		//into any depedent project
		if(this.mProjectClassPath != null) {
		
			Iterator pIt = this.mProjectClassPath.iterator();
			while(pIt.hasNext()) {
				File projectDir = (File) pIt.next();
				resultantFile = getMatchingFileInFolder(projectDir, systemId);
				
				if(resultantFile != null) {
					try {
						FileReader fReader = new FileReader(resultantFile);
						xmlSchema = parseXMLSchema(fReader, resultantFile.toURL());
					} catch(MalformedURLException ex) { 
						mLogger.log(Level.SEVERE, "Failed to parser xsd document ", ex);
						
						throw new EInsightModelException("Failed to parser xsd document "+ resultantFile, ex );
						
					} catch(FileNotFoundException ex) {
						mLogger.log(Level.SEVERE, "Failed to parser xsd document ", ex);
						
						throw new EInsightModelException("Failed to parser xsd document "+ resultantFile, ex );
					}
					
				}
			}
		}
		
		return xmlSchema;
		
	}

	
	private File getMatchingFileInFolder(File rootFolder, String fileName) {
		File file = null;
		File[] children = rootFolder.listFiles(xsdFilter);
		XSDFile[] xsdChildren = listFilesBeforeFolders(children);
		for(int i = 0; i < xsdChildren.length; i++) {
			XSDFile xsdChild = xsdChildren[i];
			File child = xsdChild.getFile();
			if(child.isDirectory()) {
				file = getMatchingFileInFolder(child, fileName);
				if(file != null) {
					break;
				}
			} else {
				
				URI matchedURI = rootFolder.toURI().resolve(fileName);
				File matchedFile = new File(matchedURI);
				if(matchedFile.exists()) {
					file = matchedFile;
					break;
				}
				
			}
		}
		return file;
	}
	
	
	//a simple implementation to list file before folders
	static XSDFile[] listFilesBeforeFolders(File[] files) {
		ArrayList newFileList = new ArrayList();
		for(int i = 0; i < files.length; i++) {
			File file = files[i];
			XSDFile xsdFile = new XSDFile(file);
			newFileList.add(xsdFile);
		}
		
		Collections.sort(newFileList);
		
		return (XSDFile[]) newFileList.toArray(new XSDFile[] {});
	}
	 
	 static class XSDFileFilter implements FileFilter {
    	
    	public boolean accept(File pathname) {
    		boolean result = false;
    		if(pathname.isDirectory()) {
    			return true;
    		}
    		
    		String fileName = pathname.getName();
    		String fileExtension = null;
    		int dotIndex = fileName.lastIndexOf('.');
    		if(dotIndex != -1) {
    			fileExtension = fileName.substring(dotIndex +1);
    		}
    		
    		if(fileExtension != null && fileExtension.equalsIgnoreCase(WSDL_FILE_EXTENSION)) {
    			result = true;
    		}
    		
    		return result;
		}
	 }
	 
	 static class XSDFile implements Comparable {
	 	private File mFile;
	 	
	 	XSDFile(File file) {
	 		mFile = file;
	 	}
	 	
	 	public File getFile() {
	 		return this.mFile;
	 	}
	 	
		/**
		 * @return  a negative integer, zero, or a positive integer as this object
		    is less than, equal to, or greater than the specified object.
		 */
		public int compareTo(Object arg0) {
			XSDFile newXSDFile = (XSDFile) arg0;
			File newFile = newXSDFile.getFile();
			
			if(newFile.isDirectory() && mFile.isFile()) {
				return -1;
			} else if (newFile.isFile() && mFile.isDirectory()) {
				return 1;
			} else {
				return mFile.compareTo(newFile);
			}
			
		}
	 }
	
}
