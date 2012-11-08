/*
 * @(#)FilesystemUtilsTest.java        $Revision: 1.2 $ $Date: 2008/12/17 23:24:45 $
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

package org.openesb.components.rules4jbi.shared.util;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/12/17 23:24:45 $
 * 
 * @since 0.3
 */
public class FilesystemUtilsTest {

    private String testDirectory = null;

    private File libDirectory = null;
    
    private File foo = null;
    
    private File bar = null;
    
    private File foobar = null;
    
    private File test = null;
    
    @Before
    public void setUp() throws MalformedURLException {
        testDirectory = System.getProperty("test.dir");
        
        libDirectory = new File(testDirectory, "lib");
        
        foo = new File(libDirectory, "foo.jar");
        
        bar = new File(libDirectory, "bar.jar");
        
        foobar = new File(libDirectory, "foobar.jar");
        
        test = new File(libDirectory, "test.dat");
    }

    @Test
    public void toURLs() throws MalformedURLException {
        URL[] result =  FilesystemUtils.toURLs(new File[] {foo, bar, test, foobar});
        
        assertEquals(4, result.length);
        
        final String libDirectoryURLString = libDirectory.toURI().toURL().toString();
        
        assertEquals(new URL(libDirectoryURLString + "foo.jar"), result[0]);
        assertEquals(new URL(libDirectoryURLString + "bar.jar"), result[1]);
        assertEquals(new URL(libDirectoryURLString + "test.dat"), result[2]);
        assertEquals(new URL(libDirectoryURLString + "foobar.jar"), result[3]);
    }
    
    @Test
    public void findAllJarFiles() {
        File[] result = FilesystemUtils.findAllJarFiles(libDirectory);
        
        assertEquals(3, result.length);

        List<File> temp = Arrays.asList(result);
        
        assertTrue(temp.contains(foo));
        assertTrue(temp.contains(bar));
        assertTrue(temp.contains(foobar));
        assertFalse(temp.contains(test));
    }
}
