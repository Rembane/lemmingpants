import babel from 'rollup-plugin-babel';
import commonjs from 'rollup-plugin-commonjs';
import nodeResolve from 'rollup-plugin-node-resolve';
import uglify from 'rollup-plugin-uglify';

export default {
    format: 'iife',
    plugins: [
        babel({ exclude: 'node_modules/**',
                presets: [
                    [
                        "env",
                        {"modules" : false}
                    ]
                ],
                //plugins: ["lodash", "external-helpers", "transform-object-rest-spread"]
             }),
        commonjs({
            include: 'node_modules/**'
        }),
        nodeResolve({ jsnext: true, main: true, browser: true })
        //uglify()
    ]
};

