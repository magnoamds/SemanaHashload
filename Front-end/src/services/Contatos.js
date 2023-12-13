import { http } from './config'

export default {

    listarTipos: () => {
        return http.get('tipos')
    },

    listar: () => {
        return http.get('contatos')
    },

    salvar: (contato) => {
        return http.post('contatos', contato)
    },

    atualizar: (contato) => {
        return http.put(`contatos/${contato.id}`, contato)
    },

    apagar: (contato) => {
        return http.delete(`contatos/${contato.id}`)
    }

}