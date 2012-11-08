 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.config;

import it.imolinfo.jbi4corba.utils.HelperFileUtil;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * Helper class for junit tests.
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a> 
 */
public final class JarListHelper {
	
	public static String CXF_VERSION = "2.1.4"; // cxf version
	public static String JBI4CORBA_VERSION = "1.0-SNAPSHOT"; // jbi4corba version
	public static String JAXB_VERSION = "2.1.9"; // jbi4corba version

	/**
	 * Gets the jar list for the JUnit tests.
	 * This jar list must be collected at runtime from the component libraries, but must 
	 * be found also during the junit test run.
	 * 
	 * @param repodir
	 * @return the jar list 
	 */
	public static List<String> extractJarList(String repodir) {

		List<String>jarFilesName = new ArrayList<String>();

		// adding needed compilation files
		jarFilesName.add(repodir + "/org/apache/cxf/cxf-rt-core/" + CXF_VERSION
				+ "/cxf-rt-core-" + CXF_VERSION + ".jar");
		jarFilesName.add(repodir + "/org/apache/cxf/cxf-api/" + CXF_VERSION
				+ "/cxf-api-" + CXF_VERSION + ".jar");
		jarFilesName.add(repodir
				+ "/org/apache/cxf/cxf-tools-wsdlto-frontend-jaxws/"
				+ CXF_VERSION + "/cxf-tools-wsdlto-frontend-jaxws-" + CXF_VERSION
				+ ".jar");
		jarFilesName.add(repodir
				+ "/org/apache/cxf/cxf-tools-wsdlto-databinding-jaxb/"
				+ CXF_VERSION + "/cxf-tools-wsdlto-databinding-jaxb-"
				+ CXF_VERSION + ".jar");
		jarFilesName.add(repodir
				+ "/org/apache/cxf/cxf-tools-wsdlto-frontend-jaxws/"
				+ CXF_VERSION + "/cxf-tools-wsdlto-frontend-jaxws-" + CXF_VERSION
				+ ".jar");
		jarFilesName.add(repodir + "/org/apache/cxf/cxf-tools-java2ws/"
				+ CXF_VERSION + "/cxf-tools-java2ws-" + CXF_VERSION + ".jar");

		// jax-ws interfaces
		jarFilesName
				.add(repodir
						+ "/org/apache/geronimo/specs/geronimo-jaxws_2.1_spec/1.0/geronimo-jaxws_2.1_spec-1.0.jar");

		jarFilesName.add(repodir + "/it/imolinfo/jbi4corba/jbi4corba/"
				+ JBI4CORBA_VERSION + "/jbi4corba-" + JBI4CORBA_VERSION + ".jar");

		// .../.m2/repository/commons-lang/commons-lang/2.1La
		String apacheCommonLangVersion = "2.4";
		jarFilesName.add(repodir + "/commons-lang/commons-lang/"
				+ apacheCommonLangVersion + "/commons-lang-"
				+ apacheCommonLangVersion + ".jar");

		// Commons collection
		String apacheCommonsCollectionVersion = "3.1";
		jarFilesName.add(repodir + "/commons-collections/commons-collections/"
				+ apacheCommonsCollectionVersion + "/commons-collections-"
				+ apacheCommonsCollectionVersion + ".jar");

		// Jaxb
		jarFilesName.add(repodir
				+ "/javax/xml/bind/jaxb-api/2.1/jaxb-api-2.1.jar");
		
		jarFilesName.add(repodir
				+ "/com/sun/xml/bind/jaxb-impl/"+JAXB_VERSION+"/jaxb-impl-"+JAXB_VERSION+".jar");
	
		jarFilesName.add(repodir
				+ "/com/sun/xml/bind/jaxb-xjc/"+JAXB_VERSION+"/jaxb-xjc-"+JAXB_VERSION+".jar");

		jarFilesName
				.add(repodir
						+ "/org/apache/geronimo/specs/geronimo-ws-metadata_2.0_spec/1.1.2/geronimo-ws-metadata_2.0_spec-1.1.2.jar");
		
		// Verify if really needed
		jarFilesName
		.add(repodir
				+ "/xalan/serializer/2.7.1/serializer-2.7.1.jar");		
		return jarFilesName;
	}
	
	/**
	 * copy all neede jars to a directory to test the SU runtime.
	 * @param repodir
	 * @param suPath
	 */
	public static void copyJarToTargetDir(String repodir, String suPath) {
				
		// Gets ths jar list
		List<String> jarFilesPath = extractJarList(repodir);
		for (String jarPath: jarFilesPath) {					
			try {
				File jarPathFile = new File(jarPath);
				String newJarName = suPath + File.separator + jarPathFile.getName();				
				HelperFileUtil.copyFile(jarPathFile, new File(newJarName));
			} catch (Exception e) {
				// Just for test pourpose...
				e.printStackTrace();
			}						
		}		
	}
	
    /**
     * Copy directory recursively.
     * 
     * @param sourceLocation
     * @param targetLocation
     * 
     * @throws IOException
     */
    public static void copyDirectory(File sourceLocation , File targetLocation)
    throws IOException {
        
        if (sourceLocation.isDirectory()) {
            if (!targetLocation.exists()) {
                targetLocation.mkdir();
            }

            String[] children = sourceLocation.list();
            for (int i=0; i<children.length; i++) {
            	copyDirectory(new File(sourceLocation, children[i]),
                              new File(targetLocation, children[i]));
            }
        } else {

            InputStream in = new FileInputStream(sourceLocation);
            OutputStream out = new FileOutputStream(targetLocation);

            // Copy the bits from instream to outstream
            byte[] buf = new byte[1024];
            int len;
            while ((len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }
            in.close();
            out.close();
        }
    }    
    


}
