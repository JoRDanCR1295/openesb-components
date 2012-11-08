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
package com.gestalt.jbi.rss.component;

import com.gestalt.jbi.nmr.NmrWrapperUtils;

import com.sun.syndication.feed.module.georss.GeoRSSModule;
import com.sun.syndication.feed.module.georss.GeoRSSUtils;
import com.sun.syndication.feed.synd.SyndContentImpl;
import com.sun.syndication.feed.synd.SyndEntry;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.Namespace;

import org.jdom.output.DOMOutputter;
import org.jdom.output.XMLOutputter;

import java.util.List;
import java.util.logging.Logger;

import javax.xml.namespace.QName;


/**
 * Takes the RSS Feed and builds the jbi wrapper 1.1 message.
 */
public class RSSNormalizer {
    private Logger log = Logger.getLogger(RSSNormalizer.class.getName());

    public org.w3c.dom.Document createConsumerContent(QName type,
        List<SyndEntry> feedList) {
        NmrWrapperUtils wrapper = new NmrWrapperUtils();
        wrapper.init(type, null);

        try {
            wrapper.addComplexType(getEntries(feedList));
        } catch (Exception e) {
            log.warning(e.toString());
        }

        return wrapper.getResult();
    }

    /**
     * Takes the syndicated feed and builds a string representation of the entries.
     *
     * @param feedList
     * @return String
     */
    @SuppressWarnings("unchecked")
    private org.w3c.dom.Element getEntries(List<SyndEntry> feedList)
        throws JDOMException {
        DOMOutputter domOutputter = new DOMOutputter();
        XMLOutputter xmlOutputter = new XMLOutputter();

        Namespace rssNamespace = Namespace.getNamespace(
                "http://xml.netbeans.org/schema/1.0/extensions/rssbc");
        Element entryListElement = new Element("EntryList", rssNamespace);
        Document doc = new Document(entryListElement);

        for (SyndEntry entry : feedList) {
            Element entryElement = new Element("Entry", rssNamespace);
            Element titleElement = new Element("title", rssNamespace);
            Element authorElement = new Element("author", rssNamespace);
            Element linkElement = new Element("link", rssNamespace);
            Element descriptionElement = new Element("description", rssNamespace);

            entryElement.addContent(titleElement);
            entryElement.addContent(linkElement);
            entryElement.addContent(descriptionElement);

            titleElement.setText(entry.getTitle());
            linkElement.setText(entry.getLink());

            String author = entry.getAuthor();
            if (author == null) {
                author = "";
            }
            entryElement.addContent(authorElement);
            authorElement.setText(author);

            if (entry.getDescription() != null) {
                descriptionElement.setText(entry.getDescription().getValue());
            } else {
                List<SyndContentImpl> content = (List<SyndContentImpl>) entry.getContents();

                if (content.size() > 0) {
                    descriptionElement.setText(content.get(0).getValue());
                } else {
                    // atom feed entry with no <content>.  if we had access to the actual atom
                    // feed, we would assert the entry <link rel="alternate" ...> was set
                    log.fine("entry had no content. setting empty description");
                }
            }

            if (entry.getPublishedDate() != null) {
                Element publishDateElement = new Element("publishDate",
                        rssNamespace);
                publishDateElement.setText(entry.getPublishedDate().toString());
                entryElement.addContent(publishDateElement);
            }

            GeoRSSModule geoRSSModule = GeoRSSUtils.getGeoRSS(entry);

            if (geoRSSModule != null) {
                log.fine("Feed contained Geometry");

                Element longitudeElement = new Element("longitude", rssNamespace);
                Element latituseElement = new Element("latitude", rssNamespace);
                longitudeElement.setText(Double.toString(
                        geoRSSModule.getPosition().getLongitude()));
                latituseElement.setText(Double.toString(
                        geoRSSModule.getPosition().getLatitude()));
                entryElement.addContent(longitudeElement);
                entryElement.addContent(latituseElement);
            }

            entryListElement.addContent(entryElement);
        }

        log.fine("Normalizer getEntries returning: " +
            xmlOutputter.outputString(entryListElement));

        return domOutputter.output(doc).getDocumentElement();
    }
}
