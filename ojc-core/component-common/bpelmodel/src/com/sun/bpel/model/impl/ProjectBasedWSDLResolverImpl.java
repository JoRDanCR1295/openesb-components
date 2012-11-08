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
 * @(#)ProjectBasedWSDLResolverImpl.java 
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
import com.sun.bpel.model.ProjectBasedWSDLResolver;
import com.sun.bpel.model.common.EInsightModelException;
import com.sun.bpel.xml.wsdl.WSDLDocument;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class ProjectBasedWSDLResolverImpl extends DefaultWSDLResolverImpl  implements ProjectBasedWSDLResolver {

	private Logger mLogger = Logger.getLogger(DefaultWSDLResolverImpl.class.getName());
	
	public static final String WSDL_FILE_EXTENSION = "wsdl";
	
	private List mProjectClassPath;
	
	
	private static WSDLFileFilter wsdlFilter = new WSDLFileFilter();
	
	public ProjectBasedWSDLResolverImpl(String baseURI, BPELParseContext parseContext) throws EInsightModelException {
		super(baseURI, parseContext);
	}
	
	public void setProjectClassPath(List pClasspath) {
		this.mProjectClassPath = pClasspath;
	}
	
	public File findWSDLFile(String bpelFilePath, String fileLocation) {
		File resultantFile = null;
		/*
		if(this.mProjectSourceDirs == null 
		   || this.mProjectSourceDirs.size() == 0) {
			
			throw new IllegalArgumentException("No project source directory is specified.");
		}
		*/
		URI baseURI = getBaseBpelURI();
		URI matchedURI = baseURI.resolve(fileLocation);
		File matchedFile = new File(matchedURI);
		if(matchedFile.exists()) {
			return matchedFile;
		}
		

		
		//no matching file found in current project , so now look 
		//into any depedent project
		if(this.mProjectClassPath != null) {
		
			Iterator pIt = this.mProjectClassPath.iterator();
			while(pIt.hasNext()) {
				File projectDir = (File) pIt.next();
				resultantFile = getMatchingFileInFolder(projectDir, fileLocation);
				
				if(resultantFile != null) {
					return resultantFile;
				}
			}
		}
		
		return null;
	}
	
	public WSDLDocument resolve(String publicId, String systemId) throws EInsightModelException {
		WSDLDocument document = null;
		
		try {
			document = super.resolve(publicId, systemId);
			if(document != null) {
				return document;
			}
		} catch(EInsightModelException ex) {
			mLogger.log(Level.WARNING, "Failed to parser wsdl document ", ex);
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
						document = parseWSDL(fReader, resultantFile.toURL());
					} catch(MalformedURLException ex) { 
						mLogger.log(Level.SEVERE, "Failed to parser wsdl document ", ex);
						
						throw new EInsightModelException("Failed to parser wsdl document "+ resultantFile, ex );
						
					} catch(FileNotFoundException ex) {
						mLogger.log(Level.SEVERE, "Failed to parser wsdl document ", ex);
						
						throw new EInsightModelException("Failed to parser wsdl document "+ resultantFile, ex );
					}
					
				}
			}
		}
		
		return document;
		
	}
	
	
	
	
	
	private File getMatchingFileInFolder(File rootFolder, String fileName) {
		File file = null;
		File[] children = rootFolder.listFiles(wsdlFilter);
		WSDLFile[] wsdlChildren = listFilesBeforeFolders(children);
		for(int i = 0; i < wsdlChildren.length; i++) {
			WSDLFile wsdlChild = wsdlChildren[i];
			File child = wsdlChild.getWSDLFile();
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
	static WSDLFile[] listFilesBeforeFolders(File[] files) {
		ArrayList newFileList = new ArrayList();
		for(int i = 0; i < files.length; i++) {
			File file = files[i];
			WSDLFile wFile = new WSDLFile(file);
			newFileList.add(wFile);
		}
		
		Collections.sort(newFileList);
		
		return (WSDLFile[]) newFileList.toArray(new WSDLFile[] {});
	}
	 
	 static class WSDLFileFilter implements FileFilter {
    	
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
	 
	 static class WSDLFile implements Comparable {
	 	private File mWsdlFile;
	 	
	 	WSDLFile(File wsdlFile) {
	 		mWsdlFile = wsdlFile;
	 	}
	 	
	 	public File getWSDLFile() {
	 		return this.mWsdlFile;
	 	}
	 	
		/**
		 * @return  a negative integer, zero, or a positive integer as this object
		    is less than, equal to, or greater than the specified object.
		 */
		public int compareTo(Object arg0) {
			WSDLFile newWSDLFile = (WSDLFile) arg0;
			File newFile = newWSDLFile.getWSDLFile();
			
			if(newFile.isDirectory() && mWsdlFile.isFile()) {
				return -1;
			} else if (newFile.isFile() && mWsdlFile.isDirectory()) {
				return 1;
			} else {
				return mWsdlFile.compareTo(newFile);
			}
			
		}
	 }
}
