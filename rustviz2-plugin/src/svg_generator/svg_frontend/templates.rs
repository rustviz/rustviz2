pub static CODE_PANEL_TEMPLATE : &'static str = 
"<svg width=\"{{tl_width}}px\" height=\"{{height}}px\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">

    <desc>{{ visualization_name }}</desc>

    <defs>
        <style type=\"text/css\">
        <![CDATA[
        {{ css }}
        ]]>
        </style>
    </defs>

    <g>
        <text id=\"caption\" x=\"30\" y=\"30\">Hover over timeline events (dots), states (vertical lines),</text>
        <text id=\"caption\" x=\"30\" y=\"50\">and actions (arrows) for extra information.</text>
    </g>

    {{ code }}

</svg>";

pub static TIMELINE_PANEL_TEMPLATE: &'static str = 
"<svg width=\"{{tl_width}}px\" height=\"{{height}}px\" 
        xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" 
        id=\"{{tl_id}}\">

    <desc>{{ visualization_name }}</desc>

    <defs>
        <style type=\"text/css\">
        <![CDATA[
        {{ css }}
        
        text {
            user-select: none;
            -webkit-user-select: none;
            -moz-user-select: none;
            -ms-user-select: none;
        }
        ]]>
        </style>
        <!-- used when pass to function by ref -->
        <g id=\"functionDot\">
             <circle cx=\"0\" cy=\"0\" r=\"5\" fill=\"transparent\"/>
             <text class=\"functionIcon\" dx=\"-3.5\" dy=\"0\" fill=\"#6e6b5e\">f</text>
        </g>
        <marker id=\"arrowHead\" viewBox=\"0 0 10 10\"
            refX=\"0\" refY=\"4\"
            markerUnits=\"strokeWidth\"
            markerWidth=\"3px\" markerHeight=\"3px\"
            orient=\"auto\" fill=\"gray\">
            <path d=\"M 0 0 L 8.5 4 L 0 8 z\" fill=\"inherit\"/>
        </marker>
        <!-- glow highlight filter -->
        <filter id=\"glow\" x=\"-5000%\" y=\"-5000%\" width=\"10000%\" height=\"10000%\" filterUnits=\"userSpaceOnUse\">
            <feComposite in=\"flood\" result=\"mask\" in2=\"SourceGraphic\" operator=\"in\"></feComposite>
            <feGaussianBlur stdDeviation=\"2\" result=\"coloredBlur\"/>
            <feMerge>
                <feMergeNode in=\"coloredBlur\"></feMergeNode>
                <feMergeNode in=\"coloredBlur\"></feMergeNode>
                <feMergeNode in=\"coloredBlur\"></feMergeNode>
                <feMergeNode in=\"SourceGraphic\"></feMergeNode>
            </feMerge>
            <!-- increase brightness -->
            <feComponentTransfer>
                <feFuncR type=\"linear\" slope=\"2\"/>
                <feFuncG type=\"linear\" slope=\"2\"/>
                <feFuncB type=\"linear\" slope=\"2\"/>
            </feComponentTransfer>
        </filter>
    </defs>

    {{ diagram }}

</svg>";


pub static CSS_TEMPLATE: &'static str = 
"/* general setup */
    :root {
        --bg-color:#f1f1f1;
        --text-color: #6e6b5e;
    }
    
    svg {
        background-color: var(--bg-color);
    }
    
    text {
        vertical-align: baseline;
        text-anchor: start;
    }
    
    #heading {
        font-size: 24px;
        font-weight: bold;
    }
    
    #caption {
        font-size: 0.875em;
        font-family: \"Open Sans\", sans-serif;
        font-style: italic;
    }
    
    /* code related styling */
    text.code {
        fill: #6e6b5e;
        white-space: pre;
        font-family: \"Source Code Pro\", Consolas, \"Ubuntu Mono\", Menlo, \"DejaVu Sans Mono\", monospace, monospace !important;
        font-size: 0.875em;
    }
    
    text.label {
        font-family: \"Source Code Pro\", Consolas, \"Ubuntu Mono\", Menlo, \"DejaVu Sans Mono\", monospace, monospace !important;
        font-size: 0.875em;
    }
    
    /* timeline/event interaction styling */
    .solid {
        stroke-width: 5px;
    }
    
    .hollow {
        stroke-width: 1.5;
    }
    
    .dotted {
        stroke-width: 5px;
        stroke-dasharray: \"2 1\";
    }
    
    .extend {
        stroke-width: 1px;
        stroke-dasharray: \"2 1\";
    }
    
    .functionIcon {
        paint-order: stroke;
        stroke-width: 3px;
        fill: var(--bg-color);
        font-size: 20px;
        font-family: times;
        font-weight: lighter;
        dominant-baseline: central;
        text-anchor: start;
        font-style: italic;
    }
    
    .functionLogo {
        font-size: 20px;
        font-style: italic;
        paint-order: stroke;
        stroke-width: 3px;
        fill: var(--bg-color) !important;
    }
    
    /* flex related styling */
    .flex-container {
        display: flex;
        flex-direction: row;
        justify-content: flex-start;
        flex-wrap: nowrap;
        flex-shrink: 0;
    }
    
    object.tl_panel {
        flex-grow: 1;
    }
    
    object.code_panel {
        flex-grow: 0;
    }
    
    .tooltip-trigger {
        cursor: default;
    }
    
    .tooltip-trigger:hover{
        filter: url(#glow);
    }
    
    /* hash based styling */
    [data-hash=\"0\"] {
        fill: #6e6b5e;
    }
    
    [data-hash=\"1\"] {
        fill: #1893ff;
        stroke: #1893ff;
    }
    
    [data-hash=\"2\"] {
        fill: #ff7f50;
        stroke: #ff7f50;
    }
    
    [data-hash=\"3\"] {
        fill: #8635ff;
        stroke: #8635ff;
    }
    
    [data-hash=\"4\"] {
        fill: #dc143c;
        stroke: #dc143c;
    }
    
    [data-hash=\"5\"] {
        fill: #0a810a;
        stroke: #0a810a;
    }
    
    [data-hash=\"6\"] {
        fill: #008080;
        stroke: #008080;
    }
    
    [data-hash=\"7\"] {
        fill: #ff6cce;
        stroke: #ff6cce;
    }
    
    [data-hash=\"8\"] {
        fill: #00d6fc;
        stroke: #00d6fc;
    }
    
    [data-hash=\"9\"] {
        fill: #b99f35;
        stroke: #b99f35;
    }
    [data-hash=\"10\"] {
        fill: #15a710;
        stroke: #15a710;
    }
    [data-hash=\"11\"] {
        fill: #000000;
        stroke: #000000;
    }
    [data-hash=\"12\"] {
        fill: #463ae9;
        stroke: #463ae9;
    }";


