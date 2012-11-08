/**
 *   rss-binding-component-extensions - Extensions for the RSS Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com/
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
 *   License along with this library if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */

package com.gestalt.jbi.rss.extensions

import javax.wsdl.extensions.ExtensionRegistry
import javax.wsdl.factory.WSDLFactory
import javax.xml.parsers.DocumentBuilderFactory
import java.io.PrintWriter
import java.io.StringWriter

class RSSExtSerializerTest extends GroovyTestCase {
    def wsdlFile = "/rssTestWsdl.wsdl"
    def extReg = new ExtensionRegistry()

    def void testMarshall() {
        def serializer = new RSSExtSerializer()
        def wsdlReader = WSDLFactory.newInstance().newWSDLReader()
        def wsdl = wsdlReader.readWSDL(this.class.getResource(wsdlFile).path)

        def ext = new RSSBinding()
        def output = new StringWriter(512)
        def writer = new PrintWriter(output)
        serializer.marshall(null, null, ext, writer, wsdl, extReg)
        writer.flush()

        assertEquals("<rss:binding/>", output.toString().trim())

        ext = new RSSAddress()
        output = new StringWriter(512)
        writer = new PrintWriter(output)
        serializer.marshall(null, null, ext, writer, wsdl, extReg)
        writer.flush()

        assertEquals("<rss:address/>", output.toString().trim())

        writer.close()
    }

    def void testUnmarshall() {
        def serializer = new RSSExtSerializer()
        def wsdl = getWsdlDocument()

        def element = getElement(wsdl, "rss:binding")
        def ext = serializer.unmarshall(null,
                RSSBinding.QNAME_BINDING, element, null, extReg)
        assertTrue(ext instanceof RSSBinding)

        element = getElement(wsdl, "rss:address")
        assertNotNull("Couldn't get address from wsdl", element)
        ext = serializer.unmarshall(null,
                RSSAddress.QNAME_ADDRESS, element, null, extReg)
        assertTrue(ext instanceof RSSAddress)
        assertEquals("http://gallemore.blogspot.com/feeds/posts/" +
                "default?alt=rss", ext.location)
        assertEquals("username", ext.username)
        assertEquals("password", ext.password)
        assertEquals("Chad's Blog", ext.feedTitle)
        assertEquals("Test Blog", ext.feedDescription)
        assertEquals("atom_1.0", ext.feedType)

        element = getElement(wsdl, "rss:operation")
        assertNotNull("Couldn't get operation from wsdl", element)
        ext = serializer.unmarshall(null,
                RSSOperation.QNAME_OPERATION, element, null, extReg)
        assertTrue(ext instanceof RSSOperation)
        assertEquals(RSSOperation.OperationName.publish,
                ext.operationName)

        element = getElement(wsdl, "rss:output")
        ext = serializer.unmarshall(null,
                RSSOperationOutput.QNAME_OPERATION_OUTPUT, element, null, extReg)
        assertTrue(ext instanceof RSSOperationOutput)

        element = getElement(wsdl, "rss:input")
        assertNotNull("Couldn't get input from wsdl", element)
        ext = serializer.unmarshall(null,
                RSSOperationInput.QNAME_OPERATION_INPUT, element, null, extReg)
        assertTrue(ext instanceof RSSOperationInput)
        assertEquals(10, ext.pollingInterval)
        assertEquals("publishDate", ext.filterByType.toString())
        assertEquals("now", ext.filterByValue)
        assertEquals("longitude", ext.longitude)
        assertEquals("latitude", ext.latitude)
        assertEquals("destinationUrl", ext.destinationUrl)
    }

    def getWsdlDocument() throws Exception {
        def dbf = DocumentBuilderFactory.newInstance()
        dbf.namespaceAware = true
        def db = dbf.newDocumentBuilder()
        def doc = db.parse(
                this.class.getResource(wsdlFile).file)

        if (doc == null) {
            fail("Something went wrong while parsing the wsdl, cannot proceed")
        }

        return doc
    }

    def getElement(aNode, elementName) {
       if (aNode.nodeName.equalsIgnoreCase(elementName)) {
           return aNode
       }

       def element = null
       for (node in aNode.childNodes) {
           if (node.nodeName.equalsIgnoreCase(elementName)) {
               return node
           } else {
               element = getElement(node, elementName)
               if (element != null) {
                   break
               }
           }
       }
       return element
    }
}