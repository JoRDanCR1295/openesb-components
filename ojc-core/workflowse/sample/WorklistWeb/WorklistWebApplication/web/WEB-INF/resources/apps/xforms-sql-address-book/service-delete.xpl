<!--
    Copyright (C) 2004 Orbeon, Inc.

    This program is free software; you can redistribute it and/or modify it under the terms of the
    GNU Lesser General Public License as published by the Free Software Foundation; either version
    2.1 of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU Lesser General Public License for more details.

    The full text of the license is available at http://www.gnu.org/copyleft/lesser.html
-->
<p:config xmlns:p="http://www.orbeon.com/oxf/pipeline"
          xmlns:sql="http://orbeon.org/oxf/xml/sql"
          xmlns:xs="http://www.w3.org/2001/XMLSchema"
          xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
          xmlns:oxf="http://www.orbeon.com/oxf/processors">

    <p:param type="input" name="instance"/>

    <!-- If necssary, create tables in database -->
    <p:processor name="oxf:pipeline">
        <p:input name="config" href="init-database/init-database.xpl"/>
    </p:processor>

    <p:processor name="oxf:sql">
        <p:input name="data" href="#instance"/>
        <p:input name="datasource" href="/config/datasource-sql.xml"/>
        <p:input name="config">
            <sql:config>
                <result>
                    <sql:connection>
                        <sql:execute>
                            <sql:update>
                                delete from orbeon_address_book where id =
                                    <sql:param type="xs:int" select="/form/id"/>
                            </sql:update>
                        </sql:execute>
                    </sql:connection>
                </result>
            </sql:config>
        </p:input>
    </p:processor>

</p:config>
