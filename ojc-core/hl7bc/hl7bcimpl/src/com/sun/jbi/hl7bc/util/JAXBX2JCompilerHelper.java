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
 * @(#)JAXBX2JCompilerHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.util;


import com.sun.tools.xjc.api.XJC;
import com.sun.tools.xjc.api.SchemaCompiler;
import com.sun.tools.xjc.api.S2JJAXBModel;
import org.xml.sax.InputSource;
import org.xml.sax.SAXParseException;
import com.sun.codemodel.JCodeModel;
import com.sun.tools.xjc.api.ErrorListener;
import java.io.File;
import java.util.logging.Logger;
import java.util.logging.Level;
import com.sun.jbi.hl7bc.I18n;
/**
 * This class allows user to set important options and invoke JAXB XJC to generate java source files
 * @author S. Nageswara Rao, Raghunadh
 *
 */
public class JAXBX2JCompilerHelper {

    private static Logger mLogger = Logger.getLogger(JAXBX2JCompilerHelper.class.getName());
	
	private static final String PACKAGE_NAME_PREFIX = "com.sun.hl7bc.ack.hl7v3."; 
	private static final String GENERATED_SRC_DIR = "gen-src";
	private static final String XSD_EXTENSION = ".xsd";
	private SchemaCompiler mSchemaCompiler;
	
	private String mDestDir;
	
	
	public JAXBX2JCompilerHelper() {
		initialize();
	}
	
	/**
	 * Initializes the options
	 *
	 */
	private void initialize() {
		mSchemaCompiler = XJC.createSchemaCompiler();
        mSchemaCompiler.setErrorListener(new MyErrorListener());
	}
	
	/**
	 * Sets the schema for which java classes are generated
	 * @param schema
	 */
	public void parseSchema(String schema) {
		mSchemaCompiler.parseSchema(new InputSource(schema));
	}
	
	/**
	 * Sets the packageName for the generated java classes
	 * @param packageName
	 */
	public void setPackage(String packageName) {
		//remove the extension from the passed in string
		if(packageName.endsWith(XSD_EXTENSION)) {
			packageName = packageName.substring(0, packageName.lastIndexOf("."));
		}
		mSchemaCompiler.forcePackageName(PACKAGE_NAME_PREFIX + packageName);
	}
	
	/**
	 * Sets the directory where the generated files will go
	 * @param destDir
	 */
	public void setDestDir(String destDir) {
		mDestDir = destDir + File.separator +  GENERATED_SRC_DIR;
    }
	
	/**
	 * Get the generated source dir 
	 * @return
	 */
	public String getGeneratedSourceDir() {
		return mDestDir;
	}
	
	/**
	 * Invokes JAXB XJC tool
	 * @throws Exception
	 */
	public void invokeXJC() throws Exception {
		S2JJAXBModel model =  mSchemaCompiler.bind();
        JCodeModel jcmodel = model.generateCode(null, null);
        File file = new File(mDestDir);
        if (!file.exists())
            file.mkdir();
        jcmodel.build(file);
	}
	
	
	class MyErrorListener implements ErrorListener {

        MyErrorListener() {

        }

        public void error(SAXParseException exception) {
            mLogger.log(Level.SEVERE, I18n.msg("E0319: Unable to parse the XSD {0}.", exception.getMessage()));
        }

        public void fatalError(SAXParseException exception) {
            mLogger.log(Level.SEVERE, I18n.msg("E0320: Getting fatal error while using JAXB {0}.", exception.getMessage()));
        }

        public void warning(SAXParseException exception) {
            mLogger.log(Level.SEVERE, I18n.msg("E0321: Warning message while parsing the XSD {0}.", exception.getMessage()));
        }

        public void info(SAXParseException exception) {
            mLogger.log(Level.SEVERE, I18n.msg("E0322: Info message while parsing the XSD {0}.", exception.getMessage()));
        }
    }
	
	
	
	
	
	
	
	
	

}
