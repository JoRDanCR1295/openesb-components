/*
 * @(#)Validator.java        $Revision: 1.2 $ $Date: 2008/12/17 23:21:34 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.util;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.regex.Pattern;

import org.openide.filesystems.FileObject;

import org.netbeans.api.java.source.JavaSource;

/**
 * Utility class for validating user input.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/12/17 23:21:34 $
 * 
 * @since 0.1
 */
public final class Validator {

    /**
     * Simplified pattern for matching noncolonized names as defined in "Namespaces in XML 1.0" spec.
     * Might reject some valid NCNames. See http://www.w3.org/TR/xml-names/ for full grammar.
     */ 
    private static final String NCNAME_PATTERN = "^[a-zA-Z_][\\w\\-\\.]*$";
    
    /**
     * Attempt to define a pattern for valid file names on all platforms.
     * Might reject some valid filenames on some/all platforms, but hopefully not accept invalid ones.
     */
    private static final String FILE_NAME_PATTERN =
            "(^\\w[^\\\\/:?*<>\"|&%#\\^@\\t\\n\\x0B\\f\\r\\x08]*\\w$)|(^\\w$)";
    
    private static final Pattern ncNamePattern = Pattern.compile(NCNAME_PATTERN);
    
    private static final Pattern fileNamePattern = Pattern.compile(FILE_NAME_PATTERN);
    
    private Validator() {}
    
    public static boolean isValidNCName(String input) {
        return input == null ? false : ncNamePattern.matcher(input).matches();
    }
    
    public static boolean isValidFileName(String input) {
        return input == null ? false : fileNamePattern.matcher(input).matches();
    }
    
    public static boolean isValidURI(String input) {
        if (input == null || input.trim().equals("")) {
            return false;
        }
        
        try {
            new URI(input);
            return true;
            
        } catch (URISyntaxException e) {
            return false;
        }
    }
    
    /**
     * Tests whether the given <code>FileObject</code> represents a java source file.
     * 
     * @param fileObject <code>FileObject</code> to test; can be <code>null</code>
     * @return true if the given <code>FileObject</code> represents a java source file
     */
    public static boolean isValidJavaSourceFile(FileObject fileObject) {
        
        /* Using JavaSource is the best way for this purpose I could come up with so far */
        return fileObject == null ? false : JavaSource.forFileObject(fileObject) != null;
    }
}
