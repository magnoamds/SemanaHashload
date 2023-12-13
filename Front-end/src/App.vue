<template>
    <body>
        <TheHeader />
        <TheHome />
        <TheFooter />
    </body>
</template>

<script>
import './assets/css/style.css'
import TheHeader from './components/TheHeader.vue'
import TheHome from './components/TheHome.vue'
import TheFooter from './components/TheFooter.vue'

export default {
    name: 'App',
    components: {
        TheHeader,
        TheHome,
        TheFooter
    }
}
</script>