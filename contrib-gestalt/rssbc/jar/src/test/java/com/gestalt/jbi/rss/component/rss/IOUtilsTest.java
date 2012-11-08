/**
 *   rss-binding-component - RSS Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.rss.component.rss;

import com.gestalt.jbi.rss.component.rss.persistence.IOUtils;

import junit.framework.TestCase;

import java.io.File;


/**
 * Created by IntelliJ IDEA.
 * User: jthorn
 * Date: Oct 26, 2007
 * Time: 3:17:46 PM
 * To change this template use File | Settings | File Templates.
 */
public class IOUtilsTest extends TestCase {
    public void testFileUtils() throws Exception {
        String message = "w00t\n!!!\n!!!";

        File file = new File("test.data");

        try {
            IOUtils.writeToFile(message, file);

            String result = IOUtils.readFile(file);

            assertEquals(result, message);
        } finally {
            file.delete();
        }
    }

    public void testObjectUtils() throws Exception {
        String message = "w00t\n!!!\n!!!";

        File file = new File("test.data");

        try {
            IOUtils.storeObject(message, file);

            String result = (String) IOUtils.loadObject(file);

            assertEquals(result, message);
        } finally {
            file.delete();
        }
    }
}
